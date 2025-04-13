import { ethers } from "ethers";
import dotenv from "dotenv";
import fs from "fs";
import solc from "solc";
import path from "path";
import blessed from "neo-blessed";
import chalk from "chalk";
import cliSpinners from "cli-spinners";
import { HttpsProxyAgent } from "https-proxy-agent";

dotenv.config();

const provider = new ethers.JsonRpcProvider(process.env.RPC_URL);
const wallet = new ethers.Wallet(process.env.PRIVATE_KEY, provider);

let savedOption = null;
let savedTransactionCount = null;

// Network configuration
const network = {
  name: "Tea Sepolia Testnet 🌐",
  rpc: "https://tea-sepolia.g.alchemy.com/public",
  chainId: 10218,
  symbol: "TEA",
  explorer: "https://sepolia.tea.xyz/",
};

// Contract ABIs
const erc20ABI = [
  "function balanceOf(address owner) view returns (uint256)",
  "function transfer(address to, uint256 amount) returns (bool)",
  "function decimals() view returns (uint8)",
];

const stTeaABI = [
  "function stake() payable",
  "function balanceOf(address owner) view returns (uint256)",
  "function withdraw(uint256 _amount)",
];

const stTeaContractAddress = "0x04290DACdb061C6C9A0B9735556744be49A64012";

// Create screen with hacker theme
const screen = blessed.screen({
  smartCSR: true,
  title: "TEA-HACK-BOT",
  cursor: { color: "#00ff00" },
});

// Main container
const container = blessed.box({
  parent: screen,
  top: 0,
  left: 0,
  width: "100%",
  height: "100%",
  style: { bg: "black", fg: "#00ff00" },
});

// Status bar
const statusBar = blessed.box({
  parent: container,
  top: 0,
  left: 0,
  width: "100%",
  height: 1,
  content: " [TEA-HACK-BOT v1.0] - SYSTEM ONLINE ",
  style: { bg: "#00ff00", fg: "black", bold: true },
});

// Log window
const logWindow = blessed.log({
  parent: container,
  top: 1,
  left: 0,
  width: "70%",
  height: "90%",
  border: { type: "line", fg: "#00ff00" },
  style: { fg: "#00ff00", bg: "black", scrollbar: { bg: "#00ff00" } },
  scrollable: true,
  scrollbar: true,
  tags: true,
  padding: { left: 1, right: 1 },
});

// Info panel
const infoPanel = blessed.box({
  parent: container,
  top: 1,
  right: 0,
  width: "30%",
  height: "90%",
  border: { type: "line", fg: "#00ff00" },
  style: { fg: "#00ff00", bg: "black" },
  content: "{center}SYSTEM INFO{/center}\n\nInitializing...",
  tags: true,
});

// Input box
const inputBox = blessed.textbox({
  parent: container,
  bottom: 0,
  left: 0,
  width: "100%",
  height: 3,
  border: { type: "line", fg: "#00ff00" },
  style: { fg: "#00ff00", bg: "black" },
  hidden: true,
  inputOnFocus: true,
});

// Key bindings
screen.key(["escape", "q", "C-c"], () => process.exit(0));

// Function to get user input
function getInput(promptText) {
  return new Promise((resolve) => {
    logWindow.log(`{yellow-fg}${promptText}{/yellow-fg}`);
    inputBox.setValue("");
    inputBox.show();
    screen.render();

    inputBox.once("submit", (value) => {
      inputBox.hide();
      screen.render();
      resolve(value);
    });

    inputBox.focus();
    inputBox.readInput();
  });
}

// Show banner
function showBanner() {
  logWindow.log("{bold}{green-fg}>>> SYSTEM BOOT SEQUENCE INITIATED{/green-fg}{/bold}");
  logWindow.log("{green-fg}[[ TEA AUTO BOT ]] - BY KAZUHA{/green-fg}");
  logWindow.log("{green-fg}DEVELOPER: t.me/Offical_Im_kazuha{/green-fg}");
  logWindow.log("{green-fg}SOURCE: github.com/Kazuha787{/green-fg}");
  logWindow.log("{green-fg}----------------------------------{/green-fg}");
  screen.render();
}

// Show wallet info
async function showWalletInfo() {
  const balance = await provider.getBalance(wallet.address);
  const stTeaContract = new ethers.Contract(stTeaContractAddress, ["function balanceOf(address owner) view returns (uint256)"], wallet);
  const stTeaBalance = await stTeaContract.balanceOf(wallet.address).catch(() => ethers.BigNumber.from(0));
  infoPanel.setContent(
    "{center}{bold}SYSTEM INFO{/bold}{/center}\n\n" +
    "{green-fg}WALLET:{/green-fg}\n" +
    `ADDR: ${wallet.address}\n` +
    `BAL: ${ethers.formatEther(balance)} ETH\n` +
    `stTEA BAL: ${ethers.formatEther(stTeaBalance)} stTEA\n\n` +
    "{green-fg}STATUS:ONLINE{/green-fg}\n" +
    "{green-fg}OK:{/green-fg}\n"
  );
  logWindow.log("{green-fg}>>> WALLET SYNC COMPLETE{/green-fg}");
  screen.render();
}

// Deploy contract
async function deployContract() {
  const contractPath = path.resolve("auto.sol");

  if (!fs.existsSync(contractPath)) {
    logWindow.log("{red-fg}[[ERROR]] auto.sol NOT FOUND{/red-fg}");
    screen.render();
    return;
  }

  const contractSource = fs.readFileSync(contractPath, "utf8");

  function findImports(importPath) {
    const fullPath = path.resolve("node_modules", importPath);
    return fs.existsSync(fullPath)
      ? { contents: fs.readFileSync(fullPath, "utf8") }
      : { error: "File not found" };
  }

  const input = {
    language: "Solidity",
    sources: { "auto.sol": { content: contractSource } },
    settings: { outputSelection: { "*": { "*": ["abi", "evm.bytecode.object"] } } },
  };

  const output = JSON.parse(solc.compile(JSON.stringify(input), { import: findImports }));
  const contractName = Object.keys(output.contracts["auto.sol"])[0];
  const contractData = output.contracts["auto.sol"][contractName];

  if (!contractData.evm.bytecode.object) {
    logWindow.log("{red-fg}[[ERROR]] COMPILATION FAILED{/red-fg}");
    screen.render();
    return;
  }

  const contractFactory = new ethers.ContractFactory(contractData.abi, contractData.evm.bytecode.object, wallet);

  logWindow.log("{yellow-fg}>>> DEPLOYING CONTRACT...{/yellow-fg}");
  screen.render();

  try {
    const contract = await contractFactory.deploy("MyToken", "MTK", 1000000, wallet.address);
    await contract.waitForDeployment();
    logWindow.log(`{green-fg}[[SUCCESS]] DEPLOYED @ ${await contract.getAddress()}{/green-fg}`);
  } catch (error) {
    logWindow.log(`{red-fg}[[ERROR]] DEPLOY FAILED: ${error.message}{/red-fg}`);
  }

  logWindow.log("{green-fg}>>> DEPLOYMENT SEQUENCE COMPLETE{/green-fg}");
  screen.render();
  process.exit(0);
}

// Handle transactions
async function autoTransaction() {
  let option = savedOption;
  let transactionCount = savedTransactionCount;

  if (option === null || transactionCount === null) {
    option = await getInput(">>> SELECT TARGET (1:Burn 2:KYC): ");
    transactionCount = await getInput(">>> ENTER TX COUNT: ");
    savedOption = option;
    savedTransactionCount = Number(transactionCount);
  }

  const file = option === "1" ? "burnAddress.txt" : "KycAddress.txt";

  if (!fs.existsSync(file)) {
    logWindow.log(`{red-fg}[[ERROR]] ${file} NOT FOUND{/red-fg}`);
    screen.render();
    return;
  }

  const addresses = fs.readFileSync(file, "utf-8").split("\n").map((addr) => addr.trim()).filter((addr) => addr);

  logWindow.log("{yellow-fg}>>> INITIATING TRANSACTION SEQUENCE{/yellow-fg}");
  screen.render();

  for (let i = 0; i < savedTransactionCount; i++) {
    const recipient = addresses[Math.floor(Math.random() * addresses.length)];
    const amount = (Math.random() * (0.09 - 0.01) + 0.01).toFixed(4);

    logWindow.log(`{cyan-fg}[[TX ${i + 1}/${savedTransactionCount}]]{/cyan-fg}`);
    logWindow.log(`{green-fg}RUNNING ${amount} ETH -> ${recipient}{/green-fg}`);

    try {
      const tx = await wallet.sendTransaction({
        to: recipient,
        value: ethers.parseEther(amount),
      });

      logWindow.log(`{green-fg}SUCCESS - HASH: ${tx.hash}{/green-fg}`);
      await tx.wait();
    } catch (error) {
      logWindow.log(`{red-fg}FAILED: ${error.message}{/red-fg}`);
    }

    logWindow.log("{yellow-fg}>>> DELAY 35s...{/yellow-fg}");
    screen.render();
    await new Promise((res) => setTimeout(res, 35000));
  }

  logWindow.log("{green-fg}>>> SEQUENCE COMPLETE - RESTARTING IN 24H{/green-fg}");
  screen.render();
  setTimeout(autoTransaction, 86400000);
}

// New functions from friend's script
async function loadProxies() {
  try {
    const data = await fs.promises.readFile("proxies.txt", "utf8");
    const proxies = data.split("\n").map((line) => line.trim()).filter((line) => line);
    if (proxies.length === 0) {
      logWindow.log("{yellow-fg}No proxies found in proxies.txt. Running without proxy.{/yellow-fg}");
      return null;
    }
    return proxies;
  } catch (error) {
    logWindow.log(`{red-fg}Error reading proxies.txt: ${error.message}{/red-fg}`);
    return null;
  }
}

function getRandomProxy(proxies) {
  if (!proxies || proxies.length === 0) return null;
  const randomIndex = Math.floor(Math.random() * proxies.length);
  return proxies[randomIndex];
}

function parseProxy(proxy) {
  if (!proxy) return null;
  let proxyUrl = proxy;
  if (!proxy.startsWith("http://") && !proxy.startsWith("https://")) {
    proxyUrl = `http://${proxy}`;
  }
  return proxyUrl;
}

function showSpinner(message) {
  const spinner = cliSpinners.dots.frames;
  let i = 0;
  const interval = setInterval(() => {
    logWindow.log(`{yellow-fg}${message} ${spinner[i++ % spinner.length]}{/yellow-fg}`);
    screen.render();
  }, 100);
  return () => {
    clearInterval(interval);
    screen.render();
  };
}

async function confirmTransaction(details) {
  let preview = "Transaction Preview:\n";
  for (const [key, value] of Object.entries(details)) {
    preview += `${key}: ${value}\n`;
  }
  logWindow.log(`{white-fg}${preview}{/white-fg}`);
  const answer = await getInput("Confirm transaction? (y/n): ");
  return answer.toLowerCase() === "y" || answer.toLowerCase() === "yes";
}

async function stakeTea(wallet, amount) {
  try {
    const amountWei = ethers.parseEther(amount.toString());
    const feeData = await wallet.provider.getFeeData();
    const gasPrice = feeData.gasPrice;
    const estimatedGas = 200000;
    const gasCost = ethers.formatEther(gasPrice * BigInt(estimatedGas));

    const confirmed = await confirmTransaction({
      Action: "Stake",
      Amount: `${amount} TEA`,
      "Est. Gas": `${gasCost} TEA`,
    });

    if (!confirmed) {
      logWindow.log("{red-fg}Transaction canceled.{/red-fg}");
      logWindow.log("{white-fg}===== STAKING CANCELED ====={/white-fg}");
      screen.render();
      return null;
    }

    const stTeaContract = new ethers.Contract(stTeaContractAddress, stTeaABI, wallet);

    logWindow.log("{white-fg}===== STAKING TEA ====={/white-fg}");
    logWindow.log(`{yellow-fg}Staking ${amount} TEA...{/yellow-fg}`);

    const tx = await stTeaContract.stake({
      value: amountWei,
      gasLimit: estimatedGas,
    });

    logWindow.log(`{white-fg}Transaction sent! Hash: ${tx.hash}{/white-fg}`);
    logWindow.log(`{gray-fg}View on explorer: ${network.explorer}/tx/${tx.hash}{/gray-fg}`);

    const stopSpinner = showSpinner("Waiting for confirmation...");
    const receipt = await tx.wait();
    stopSpinner();

    logWindow.log(`{green-fg}Transaction confirmed in block ${receipt.blockNumber}{/green-fg}`);
    logWindow.log(`{green-fg}Successfully staked ${amount} TEA!{/green-fg}`);
    logWindow.log("{white-fg}===== STAKING COMPLETED ====={/white-fg}");
    screen.render();

    return receipt;
  } catch (error) {
    logWindow.log(`{red-fg}Error staking TEA: ${error.message}{/red-fg}`);
    logWindow.log("{white-fg}===== STAKING FAILED ====={/white-fg}");
    screen.render();
    return null;
  }
}

async function withdrawTea(wallet, amount) {
  try {
    const amountWei = ethers.parseEther(amount.toString());
    const feeData = await wallet.provider.getFeeData();
    const gasPrice = feeData.gasPrice;
    const estimatedGas = 100000;
    const gasCost = ethers.formatEther(gasPrice * BigInt(estimatedGas));

    const confirmed = await confirmTransaction({
      Action: "Withdraw",
      Amount: `${amount} stTEA`,
      "Est. Gas": `${gasCost} TEA`,
    });

    if (!confirmed) {
      logWindow.log("{red-fg}Transaction canceled.{/red-fg}");
      logWindow.log("{white-fg}===== WITHDRAW CANCELED ====={/white-fg}");
      screen.render();
      return null;
    }

    const stTeaContract = new ethers.Contract(stTeaContractAddress, stTeaABI, wallet);

    logWindow.log("{white-fg}===== WITHDRAWING TEA ====={/white-fg}");
    logWindow.log(`{yellow-fg}Withdrawing ${amount} stTEA...{/yellow-fg}`);

    const tx = await stTeaContract.withdraw(amountWei, {
      gasLimit: estimatedGas,
    });

    logWindow.log(`{white-fg}Transaction sent! Hash: ${tx.hash}{/white-fg}`);
    logWindow.log(`{gray-fg}View on explorer: ${network.explorer}/tx/${tx.hash}{/gray-fg}`);

    const stopSpinner = showSpinner("Waiting for confirmation...");
    const receipt = await tx.wait();
    stopSpinner();

    logWindow.log(`{green-fg}Transaction confirmed in block ${receipt.blockNumber}{/green-fg}`);
    logWindow.log(`{green-fg}Successfully withdrawn ${amount} stTEA!{/green-fg}`);
    logWindow.log("{white-fg}===== WITHDRAW COMPLETED ====={/white-fg}");
    screen.render();

    return receipt;
  } catch (error) {
    logWindow.log(`{red-fg}Error withdrawing TEA: ${error.message}{/red-fg}`);
    logWindow.log("{white-fg}===== WITHDRAW FAILED ====={/white-fg}");
    screen.render();
    return null;
  }
}

async function claimRewards(wallet) {
  try {
    logWindow.log("{white-fg}===== CLAIMING REWARDS ====={/white-fg}");
    logWindow.log("{yellow-fg}Claiming stTEA rewards...{/yellow-fg}");

    const data = "0x3d18b912";
    const feeData = await wallet.provider.getFeeData();
    const gasPrice = feeData.gasPrice;
    const estimatedGas = 100000;
    const gasCost = ethers.formatEther(gasPrice * BigInt(estimatedGas));

    const confirmed = await confirmTransaction({
      Action: "Claim Rewards",
      "Est. Gas": `${gasCost} TEA`,
    });

    if (!confirmed) {
      logWindow.log("{red-fg}Transaction canceled.{/red-fg}");
      logWindow.log("{white-fg}===== CLAIM CANCELED ====={/white-fg}");
      screen.render();
      return null;
    }

    const tx = await wallet.sendTransaction({
      to: stTeaContractAddress,
      data: data,
      gasLimit: estimatedGas,
    });

    logWindow.log(`{white-fg}Transaction sent! Hash: ${tx.hash}{/white-fg}`);
    logWindow.log(`{gray-fg}View on explorer: ${network.explorer}/tx/${tx.hash}{/gray-fg}`);

    const stopSpinner = showSpinner("Waiting for confirmation...");
    const receipt = await tx.wait();
    stopSpinner();

    logWindow.log(`{green-fg}Transaction confirmed in block ${receipt.blockNumber}{/green-fg}`);
    logWindow.log("{green-fg}Successfully claimed rewards!{/green-fg}");
    logWindow.log("{white-fg}===== CLAIMING COMPLETED ====={/white-fg}");

    const balance = await wallet.provider.getBalance(wallet.address);
    logWindow.log(`{white-fg}Updated TEA Balance: ${ethers.formatEther(balance)} ${network.symbol}{/white-fg}`);
    screen.render();

    return receipt;
  } catch (error) {
    logWindow.log(`{red-fg}Error claiming rewards: ${error.message}{/red-fg}`);
    logWindow.log("{white-fg}===== CLAIMING FAILED ====={/white-fg}");
    screen.render();
    return null;
  }
}

function generateRandomAddress() {
  const wallet = ethers.Wallet.createRandom();
  return wallet.address;
}

async function sendToRandomAddress(wallet, amount, skipConfirmation = false) {
  try {
    const toAddress = generateRandomAddress();
    const amountWei = ethers.parseEther(amount.toString());
    const feeData = await wallet.provider.getFeeData();
    const gasPrice = feeData.gasPrice;
    const estimatedGas = 21000;
    const gasCost = ethers.formatEther(gasPrice * BigInt(estimatedGas));

    if (!skipConfirmation) {
      const confirmed = await confirmTransaction({
        Action: "Transfer",
        Amount: `${amount} TEA`,
        To: toAddress.slice(0, 6) + "..." + toAddress.slice(-4),
        "Est. Gas": `${gasCost} TEA`,
      });

      if (!confirmed) {
        logWindow.log("{red-fg}Transaction canceled.{/red-fg}");
        screen.render();
        return null;
      }
    }

    logWindow.log(`{yellow-fg}Sending ${amount} TEA to random address: ${toAddress}{/yellow-fg}`);

    const tx = await wallet.sendTransaction({
      to: toAddress,
      value: amountWei,
      gasLimit: estimatedGas,
    });

    logWindow.log(`{white-fg}Transaction sent! Hash: ${tx.hash}{/white-fg}`);
    logWindow.log(`{gray-fg}View on explorer: ${network.explorer}/tx/${tx.hash}{/gray-fg}`);

    const stopSpinner = showSpinner("Waiting for confirmation...");
    const receipt = await tx.wait();
    stopSpinner();

    logWindow.log(`{green-fg}Transaction confirmed in block ${receipt.blockNumber}{/green-fg}`);
    screen.render();

    return { receipt, toAddress };
  } catch (error) {
    logWindow.log(`{red-fg}Error sending TEA: ${error.message}{/red-fg}`);
    screen.render();
    return null;
  }
}

async function executeRandomTransfers(wallet, amount, numberOfTransfers) {
  logWindow.log("{white-fg}===== BATCH TRANSFER ====={/white-fg}");
  logWindow.log(`{yellow-fg}Preparing ${numberOfTransfers} random transfers of ${amount} TEA each...{/yellow-fg}`);

  const feeData = await wallet.provider.getFeeData();
  const gasPrice = feeData.gasPrice;
  const estimatedGas = 21000;
  const gasCost = ethers.formatEther(gasPrice * BigInt(estimatedGas) * BigInt(numberOfTransfers));

  const confirmed = await confirmTransaction({
    Action: "Batch Transfer",
    "Total Amount": `${(amount * numberOfTransfers).toFixed(4)} TEA`,
    Transfers: numberOfTransfers,
    "Est. Gas": `${gasCost} TEA`,
  });

  if (!confirmed) {
    logWindow.log("{red-fg}Transaction canceled.{/red-fg}");
    logWindow.log("{white-fg}===== BATCH TRANSFER CANCELED ====={/white-fg}");
    screen.render();
    return [];
  }

  logWindow.log(`{yellow-fg}Starting ${numberOfTransfers} transfers...{/yellow-fg}`);

  const results = [];

  for (let i = 0; i < numberOfTransfers; i++) {
    logWindow.log(`{white-fg}Transfer ${i + 1}/${numberOfTransfers}{/white-fg}`);
    const result = await sendToRandomAddress(wallet, amount, true); // Fixed: Changed walletA to wallet

    if (result) {
      results.push(result);
    }

    if (i < numberOfTransfers - 1) {
      await new Promise((resolve) => setTimeout(resolve, 2000));
    }
  }

  logWindow.log(`{green-fg}Completed ${results.length}/${numberOfTransfers} transfers successfully.{/green-fg}`);
  logWindow.log("{white-fg}===== BATCH TRANSFER COMPLETED ====={/white-fg}");
  screen.render();

  return results;
}

async function executeDailyTask(wallet) {
  const amount = 0.0001;
  const numberOfTransfers = 100;

  logWindow.log("{white-fg}===== DAILY TASK ====={/white-fg}");
  logWindow.log(`{yellow-fg}Preparing daily task: ${numberOfTransfers} transfers of ${amount} TEA each{/yellow-fg}`);

  const feeData = await wallet.provider.getFeeData();
  const gasPrice = feeData.gasPrice;
  const estimatedGas = 21000;
  const gasCost = ethers.formatEther(gasPrice * BigInt(estimatedGas) * BigInt(numberOfTransfers));

  const confirmed = await confirmTransaction({
    Action: "Daily Task",
    "Total Amount": `${(amount * numberOfTransfers).toFixed(4)} TEA`,
    Transfers: numberOfTransfers,
    "Est. Gas": `${gasCost} TEA`,
  });

  if (!confirmed) {
    logWindow.log("{red-fg}Transaction canceled.{/red-fg}");
    logWindow.log("{white-fg}===== DAILY TASK CANCELED ====={/white-fg}");
    screen.render();
    return;
  }

  await executeRandomTransfers(wallet, amount, numberOfTransfers);

  logWindow.log("{white-fg}===== DAILY TASK COMPLETED ====={/white-fg}");
  screen.render();
}

// Main process
async function startProcess() {
  showBanner();
  await showWalletInfo();

  logWindow.log("{magenta-fg}>>> SELECT OPERATION:{/magenta-fg}");
  logWindow.log("{green-fg}1: DEPLOY CONTRACT{/green-fg}");
  logWindow.log("{green-fg}2: AUTO TRANX{/green-fg}");
  logWindow.log("{green-fg}3: STAKE TEA{/green-fg}");
  logWindow.log("{green-fg}4: WITHDRAW stTEA{/green-fg}");
  logWindow.log("{green-fg}5: CLAIM REWARDS{/green-fg}");
  logWindow.log("{green-fg}6: RANDOM TRANSFERS{/green-fg}");
  logWindow.log("{green-fg}7: DAILY TASK{/green-fg}");

  const choice = await getInput(">>> ENTER COMMAND: ");

  if (choice === "1") {
    await deployContract();
  } else if (choice === "2") {
    await autoTransaction();
  } else if (choice === "3") {
    const amount = await getInput("Enter amount of TEA to stake: ");
    const parsedAmount = parseFloat(amount);
    if (isNaN(parsedAmount) || parsedAmount <= 0) {
      logWindow.log("{red-fg}Invalid amount. Please enter a positive number.{/red-fg}");
      screen.render();
    } else {
      await stakeTea(wallet, parsedAmount);
    }
    setTimeout(startProcess, 3000);
  } else if (choice === "4") {
    const amount = await getInput("Enter amount of stTEA to withdraw: ");
    const parsedAmount = parseFloat(amount);
    if (isNaN(parsedAmount) || parsedAmount <= 0) {
      logWindow.log("{red-fg}Invalid amount. Please enter a positive number.{/red-fg}");
      screen.render();
    } else {
      await withdrawTea(wallet, parsedAmount);
    }
    setTimeout(startProcess, 3000);
  } else if (choice === "5") {
    await claimRewards(wallet);
    setTimeout(startProcess, 3000);
  } else if (choice === "6") {
    const amount = await getInput("Enter amount of TEA to send in each transfer: ");
    const parsedAmount = parseFloat(amount);
    if (isNaN(parsedAmount) || parsedAmount <= 0) {
      logWindow.log("{red-fg}Invalid amount. Please enter a positive number.{/red-fg}");
      screen.render();
    } else {
      const count = await getInput("Enter number of transfers to make: ");
      const parsedCount = parseInt(count);
      if (isNaN(parsedCount) || parsedCount <= 0) {
        logWindow.log("{red-fg}Invalid count. Please enter a positive integer.{/red-fg}");
        screen.render();
      } else {
        await executeRandomTransfers(wallet, parsedAmount, parsedCount);
      }
    }
    setTimeout(startProcess, 3000);
  } else if (choice === "7") {
    await executeDailyTask(wallet);
    setTimeout(startProcess, 3000);
  } else {
    logWindow.log("{red-fg}[[ERROR]] INVALID COMMAND{/red-fg}");
    screen.render();
    setTimeout(startProcess, 3000);
  }
}

// Start
startProcess();
