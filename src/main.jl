# ╔════════════════════════════════════════════════════╗
# ║          WHO IS KAZUHA? — A JULIA SHOWCASE         ║
# ║            Written in honor of the script god      ║
# ╚════════════════════════════════════════════════════╝

using Dates, Random

# ╔════════════════════════════════════════════╗
# ║             Custom Kazuha Banner           ║
# ╚════════════════════════════════════════════╝

function print_banner()
    println("╔════════════════════════════════════════════════════════════╗")
    println("║                       [ KAZUHA SCRIPTS ]                   ║")
    println("║   Telegram: https://t.me/Offical_Im_kazuha                ║")
    println("║   GitHub: https://github.com/Kazuha787                    ║")
    println("╠════════════════════════════════════════════════════════════╣")
    println("║  ██╗  ██╗ █████╗ ███████╗██╗   ██╗██╗  ██╗ █████╗          ║")
    println("║  ██║ ██╔╝██╔══██╗╚══███╔╝██║   ██║██║  ██║██╔══██╗         ║")
    println("║  █████╔╝ ███████║  ███╔╝ ██║   ██║███████║███████║         ║")
    println("║  ██╔═██╗ ██╔══██║ ███╔╝  ██║   ██║██╔══██║██╔══██║         ║")
    println("║  ██║  ██╗██║  ██║███████╗╚██████╔╝██║  ██║██║  ██║         ║")
    println("║  ╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝ ╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝         ║")
    println("╚════════════════════════════════════════════════════════════╝\n")
end

# ╔════════════════════════════════════════════╗
# ║        Kazuha Info & Script Count          ║
# ╚════════════════════════════════════════════╝

const birth_of_scripts = Date(2022, 1, 1) # set your script journey start date

function show_legend_stats()
    today = Dates.today()
    script_age = today - birth_of_scripts
    script_count = 150 + rand(50:150)

    println(">> Script Veteran Since: $(birth_of_scripts)")
    println(">> Days in the Game: $(script_age.value)")
    println(">> Total Scripts Made (est.): $script_count")
end

# ╔════════════════════════════════════════════╗
# ║          Show off Favorite Projects        ║
# ╚════════════════════════════════════════════╝

function show_projects()
    projects = [
        "MegaEth Auto Bot – Eth farming, colorful AF",
        "Oyachat Bot – Register while sipping tea",
        "Blockmesh Bot – Terminal animations with points zoomin’",
        "Monad Testnet Auto – Faucet spammer deluxe",
        "DataQuest Referrals – Real MVP for testnet hustles",
        "AriChain Signup – Email + OTP grabber king",
        "Synthelix Hunter – Waging war on rate limits"
    ]
    println("\n== Top Projects ==")
    for p in projects
        println(">> ", p)
    end
end

# ╔════════════════════════════════════════════╗
# ║           Quote Generator (Kazuha's Vibe)  ║
# ╚════════════════════════════════════════════╝

function random_quote()
    quotes = [
        "Code fast, automate everything.",
        "Why click when you can script?",
        "Bots don’t sleep. Neither do I.",
        "Proxy? No. Speed? Yes.",
        "Captcha solved. Problem dissolved.",
        "Simplicity is king — until you need points fast.",
        "One script to rule them all.",
        "Manual is for peasants."
    ]
    return rand(quotes)
end

# ╔════════════════════════════════════════════╗
# ║               Main Program Loop            ║
# ╚════════════════════════════════════════════╝

function about_me()
    print_banner()
    println(">> Loading stats about: KAZUHA (script overlord)")
    sleep(1)
    show_legend_stats()
    sleep(1)
    show_projects()
    sleep(1)
    println("\n>> Kazuha's Vibe Today:")
    println("> ", random_quote())
end

# Run it
about_me()
