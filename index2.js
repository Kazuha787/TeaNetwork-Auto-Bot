import { ethers } from "ethers";
import dotenv from "dotenv";
import fs from "fs";
import solc from "solc";
import path from "path";
import blessed from "neo-blessed";

dotenv.config();

const provider = new ethers.JsonRpcProvider(process.env.RPC_URL);
const wallet = new ethers.Wallet(process.env.PRIVATE_KEY, provider);

let savedOption = null;
let savedTransactionCount = null;

// Create screen with hacker theme
const screen = blessed.screen({
    smartCSR: true,
    title: "TEA-HACK-BOT",
    cursor: { color: "#00ff00" }
});

// Main container
const container = blessed.box({
    parent: screen,
    top: 0,
    left: 0,
    width: "100%",
    height: "100%",
    style: { bg: "black", fg: "#00ff00" }
});

// Status bar
const statusBar = blessed.box({
    parent: container,
    top: 0,
    left: 0,
    width: "100%",
    height: 1,
    content: " [TEA-HACK-BOT v1.0] - SYSTEM ONLINE ",
    style: { bg: "#00ff00", fg: "black", bold: true }
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
    padding: { left: 1, right: 1 }
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
    tags: true
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
    inputOnFocus: true
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
    infoPanel.setContent(
        "{center}{bold}SYSTEM INFO{/bold}{/center}\n\n" +
        "{green-fg}WALLET:{/green-fg}\n" +
        `ADDR: ${wallet.address}\n` +
        `BAL: ${ethers.formatEther(balance)} ETH\n\n` +
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
        return fs.existsSync(fullPath) ? 
            { contents: fs.readFileSync(fullPath, "utf8") } : 
            { error: "File not found" };
    }

    const input = {
        language: "Solidity",
        sources: { "auto.sol": { content: contractSource } },
        settings: { outputSelection: { "*": { "*": ["abi", "evm.bytecode.object"] } } }
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

    const addresses = fs.readFileSync(file, "utf-8").split("\n").map(addr => addr.trim()).filter(addr => addr);

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
                value: ethers.parseEther(amount)
            });

            logWindow.log(`{green-fg}SUCCESS - HASH: ${tx.hash}{/green-fg}`);
            await tx.wait();
        } catch (error) {
            logWindow.log(`{red-fg}FAILED: ${error.message}{/red-fg}`);
        }

        logWindow.log("{yellow-fg}>>> DELAY 35s...{/yellow-fg}");
        screen.render();
        await new Promise(res => setTimeout(res, 35000));
    }

    logWindow.log("{green-fg}>>> SEQUENCE COMPLETE - RESTARTING IN 24H{/green-fg}");
    screen.render();
    setTimeout(autoTransaction, 86400000);
}

// Main process
async function startProcess() {
    showBanner();
    await showWalletInfo();

    logWindow.log("{magenta-fg}>>> SELECT OPERATION:{/magenta-fg}");
    logWindow.log("{green-fg}1: DEPLOY CONTRACT{/green-fg}");
    logWindow.log("{green-fg}2: AUTO TRANX{/green-fg}");

    const choice = await getInput(">>> ENTER COMMAND: ");

    if (choice === "1") {
        await deployContract();
    } else if (choice === "2") {
        await autoTransaction();
    } else {
        logWindow.log("{red-fg}[[ERROR]] INVALID COMMAND{/red-fg}");
        screen.render();
        setTimeout(startProcess, 3000);
    }
}

// Start
startProcess();
