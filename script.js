document.addEventListener('DOMContentLoaded', () => {
    let wordList = [
        "aback", "abase", "abate", "abbey", "abbot", "abhor", "abide", "abled", "abode", "abort", "about", "above", "abuse", "abyss", "acorn", "acrid", "actor", "acute", "adage", "adapt", "adieu",
        "adept", "admin", "admit", "adobe", "adopt", "adore", "adorn", "adult", "affix", "afire", "afoot", "afoul", "after", "again", "agape", "agate", "agent", "agile", "aging", "aglow",
        "agony", "agree", "ahead", "aider", "aisle", "alarm", "album", "alert", "algae", "alibi", "alien", "align", "alike", "alive", "allay", "alley", "allot", "allow", "alloy", "aloft",
        "alone", "along", "aloof", "aloud", "alpha", "altar", "alter", "amass", "amaze", "amber", "amble", "amend", "amiss", "amity", "among", "amply", "amuse", "angel", "anger", "angle",
        "angry", "angst", "anime", "ankle", "annex", "annoy", "annul", "anode", "antic", "anvil", "aorta", "apart", "aphid", "aping", "apnea", "apple", "apply", "apron", "aptly", "arbor",
        "ardor", "arena", "argue", "arise", "armor", "aroma", "arose", "array", "arrow", "arson", "artsy", "ascot", "ashen", "aside", "askew", "assay", "asset", "atoll", "atone", "attic",
        "audio", "audit", "augur", "aunty", "avail", "avert", "avian", "avoid", "await", "awake", "award", "aware", "awash", "awful", "awoke", "axial", "axiom", "axion", "azure", "bacon",
        "badge", "badly", "bagel", "baggy", "baker", "baler", "balmy", "banal", "banjo", "barge", "baron", "basal", "basic", "basil", "basin", "basis", "baste", "batch", "bathe", "baton",
        "batty", "bawdy", "bayou", "beach", "beady", "beard", "beast", "beech", "beefy", "befit", "began", "begat", "beget", "begin", "begun", "being", "belch", "belie", "belle", "belly",
        "below", "bench", "beret", "berry", "berth", "beset", "betel", "bevel", "bezel", "bible", "bicep", "biddy", "bigot", "bilge", "billy", "binge", "bingo", "biome", "birch", "birth",
        "bison", "bitty", "black", "blade", "blame", "bland", "blank", "blare", "blast", "blaze", "bleak", "bleat", "bleed", "bleep", "blend", "bless", "blimp", "blind", "blink", "bliss",
        "blitz", "bloat", "block", "bloke", "blond", "blood", "bloom", "blown", "bluer", "bluff", "blunt", "blurb", "blurt", "blush", "board", "boast", "bobby", "boney", "bongo", "bonus",
        "booby", "boost", "booth", "booty", "booze", "boozy", "borax", "borne", "bosom", "bossy", "botch", "bough", "boule", "bound", "bowel", "boxer", "brace", "braid", "brain", "brake",
        "brand", "brash", "brass", "brave", "bravo", "brawl", "brawn", "bread", "break", "breed", "briar", "bribe", "brick", "bride", "brief", "brine", "bring", "brink", "briny", "brisk",
        "broad", "broil", "broke", "brood", "brook", "broom", "broth", "brown", "brunt", "brush", "brute", "buddy", "budge", "buggy", "bugle", "build", "built", "bulge", "bulky", "bully",
        "bunch", "bunny", "burly", "burnt", "burst", "bused", "bushy", "butch", "butte", "buxom", "buyer", "bylaw", "cabal", "cabby", "cabin", "cable", "cacao", "cache", "cacti", "caddy",
        "cadet", "cagey", "cairn", "camel", "cameo", "canal", "candy", "canny", "canon", "caper", "caput", "carat", "cargo", "carny", "carol", "carry", "carve", "caste", "catch", "cater",
        "catty", "caulk", "cause", "cavil", "cease", "cedar", "cello", "chafe", "chaff", "chain", "chair", "chalk", "champ", "chant", "chaos", "chard", "charm", "chart", "chase", "chasm",
        "cheap", "cheat", "check", "cheek", "cheer", "chess", "chest", "chick", "chide", "chief", "child", "chili", "chill", "chime", "china", "chirp", "chock", "choir", "choke", "chord",
        "chore", "chose", "chuck", "chump", "chunk", "churn", "chute", "cider", "cigar", "cinch", "circa", "civic", "civil", "clack", "claim", "clamp", "clang", "clank", "clash", "clasp",
        "class", "clean", "clear", "cleat", "cleft", "clerk", "click", "cliff", "climb", "cling", "clink", "cloak", "clock", "clone", "close", "cloth", "cloud", "clout", "clove", "clown",
        "cluck", "clued", "clump", "clung", "coach", "coast", "cobra", "cocoa", "colon", "color", "comet", "comfy", "comic", "comma", "conch", "condo", "conic", "copse", "coral", "corer",
        "corny", "couch", "cough", "could", "count", "coupe", "court", "coven", "cover", "covet", "covey", "cower", "coyly", "crack", "craft", "cramp", "crane", "crank", "crash", "crass",
        "crate", "crave", "crawl", "craze", "crazy", "creak", "cream", "credo", "creed", "creek", "creep", "creme", "crepe", "crept", "cress", "crest", "crick", "cried", "crier", "crime",
        "crimp", "crisp", "croak", "crock", "crone", "crony", "crook", "cross", "croup", "crowd", "crown", "crude", "cruel", "crumb", "crump", "crush", "crust", "crypt", "cubic", "cumin",
        "curio", "curly", "curry", "curse", "curve", "curvy", "cutie", "cyber", "cycle", "cynic", "daddy", "daily", "dairy", "daisy", "dally", "dance", "dandy", "datum", "daunt", "dealt",
        "death", "debar", "debit", "debug", "debut", "decal", "decay", "decor", "decoy", "decry", "defer", "deign", "deity", "delay", "delta", "delve", "demon", "demur", "denim", "dense",
        "depot", "depth", "derby", "deter", "detox", "deuce", "devil", "diary", "dicey", "digit", "dilly", "dimly", "diner", "dingo", "dingy", "diode", "dirge", "dirty", "disco", "ditch",
        "ditto", "ditty", "diver", "dizzy", "dodge", "dodgy", "dogma", "doing", "dolly", "donor", "donut", "dopey", "doubt", "dough", "dowel", "downy", "dowry", "dozen", "draft", "drain",
        "drake", "drama", "drank", "drape", "drawl", "drawn", "dread", "dream", "dress", "dried", "drier", "drift", "drill", "drink", "drive", "droit", "droll", "drone", "drool", "droop",
        "dross", "drove", "drown", "druid", "drunk", "dryer", "dryly", "duchy", "dully", "dummy", "dumpy", "dunce", "duvet", "dwarf", "dwell", "dwelt", "dying", "eager", "eagle", "early",
        "earth", "easel", "eaten", "eater", "ebony", "eclat", "edict", "edify", "eerie", "egret", "eight", "eject", "eking", "elate", "elbow", "elder", "elect", "elegy", "elfin", "elide",
        "elite", "elope", "elude", "email", "embed", "ember", "emcee", "empty", "enact", "endow", "enema", "enemy", "enjoy", "ennui", "ensue", "enter", "entry", "envoy", "epoch", "epoxy",
        "equal", "equip", "erase", "erect", "erode", "error", "erupt", "essay", "ester", "ether", "ethic", "ethos", "etude", "evade", "event", "every", "evict", "evoke", "exact", "exalt",
        "excel", "exert", "exile", "exist", "expel", "extol", "extra", "exult", "eying", "fable", "facet", "faint", "fairy", "faith", "false", "fancy", "fanny", "farce", "fatal", "fatty",
        "fault", "fauna", "favor", "feast", "fecal", "feign", "fella", "felon", "femme", "femur", "fence", "feral", "ferry", "fetal", "fetch", "fever", "fewer", "fiber", "ficus", "field",
        "fiend", "fiery", "fifth", "fifty", "fight", "filer", "filet", "filly", "filmy", "filth", "final", "finch", "finer", "first", "fishy", "fixer", "fizzy", "fjord", "flack", "flail",
        "flair", "flake", "flaky", "flame", "flank", "flare", "flash", "flask", "fleck", "fleet", "flesh", "flick", "flier", "fling", "flint", "flirt", "float", "flock", "flood", "floor",
        "flora", "floss", "flour", "flout", "flown", "fluff", "fluid", "fluke", "flume", "flung", "flunk", "flush", "flute", "flyer", "foamy", "focal", "focus", "foggy", "foist", "folio",
        "folly", "foray", "force", "forge", "forgo", "forte", "forth", "forty", "forum", "found", "foyer", "frail", "frame", "frank", "fraud", "freak", "freed", "freer", "fresh", "friar",
        "fried", "frill", "frisk", "fritz", "frock", "frond", "front", "frost", "froth", "frown", "froze", "fruit", "fudge", "fugue", "fully", "fungi", "funky", "funny", "furor", "furry",
        "fussy", "fuzzy", "gaffe", "gaily", "gamer", "gamma", "gamut", "gassy", "gaudy", "gauge", "gaunt", "gauze", "gavel", "gawky", "gayer", "gayly", "gazer", "gecko", "geeky", "geese",
        "genie", "genre", "ghost", "ghoul", "giant", "giddy", "gipsy", "girly", "girth", "given", "giver", "glade", "gland", "glare", "glass", "glaze", "gleam", "glean", "glide", "glint",
        "gloat", "globe", "gloom", "glory", "gloss", "glove", "glyph", "gnash", "gnome", "godly", "going", "golem", "golly", "gonad", "goner", "goody", "gooey", "goofy", "goose", "gorge",
        "gouge", "gourd", "grace", "grade", "graft", "grail", "grain", "grand", "grant", "grape", "graph", "grasp", "grass", "grate", "grave", "gravy", "graze", "great", "greed", "green",
        "greet", "grief", "grill", "grime", "grimy", "grind", "gripe", "groan", "groin", "groom", "grope", "gross", "group", "grout", "grove", "growl", "grown", "gruel", "gruff", "grunt",
        "guard", "guava", "guess", "guest", "guide", "guild", "guile", "guilt", "guise", "gulch", "gully", "gumbo", "gummy", "guppy", "gusto", "gusty", "gypsy", "habit", "hairy", "halve",
        "handy", "happy", "hardy", "harem", "harpy", "harry", "harsh", "haste", "hasty", "hatch", "hater", "haunt", "haute", "haven", "havoc", "hazel", "heady", "heard", "heart", "heath",
        "heave", "heavy", "hedge", "hefty", "heist", "helix", "hello", "hence", "heron", "hilly", "hinge", "hippo", "hippy", "hitch", "hoard", "hobby", "hoist", "holly", "homer", "honey",
        "honor", "horde", "horny", "horse", "hotel", "hotly", "hound", "house", "hovel", "hover", "howdy", "human", "humid", "humor", "humph", "humus", "hunch", "hunky", "hurry", "husky",
        "hussy", "hutch", "hydro", "hyena", "hymen", "hyper", "icily", "icing", "ideal", "idiom", "idiot", "idler", "idyll", "igloo", "iliac", "image", "imbue", "impel", "imply", "inane",
        "inbox", "incur", "index", "inept", "inert", "infer", "ingot", "inlay", "inlet", "inner", "input", "inter", "intro", "ionic", "irate", "irony", "islet", "issue", "ivory", "jaunt",
        "jazzy", "jelly", "jerky", "jetty", "jewel", "jiffy", "joint", "joist", "joker", "jolly", "joust", "judge", "juice", "juicy", "jumbo", "jumpy", "junta", "junto", "juror", "kappa",
        "karma", "kayak", "kebab", "khaki", "kinky", "kiosk", "kitty", "knack", "knave", "knead", "kneed", "kneel", "knelt", "knife", "knock", "knoll", "known", "koala", "krill", "label",
        "labor", "laden", "ladle", "lager", "lance", "lanky", "lapel", "lapse", "large", "larva", "lasso", "latch", "later", "lathe", "latte", "laugh", "layer", "leach", "leafy", "leaky",
        "leant", "leapt", "learn", "lease", "leash", "least", "leave", "ledge", "leech", "leery", "lefty", "legal", "leggy", "lemon", "lemur", "leper", "level", "lever", "libel", "liege",
        "light", "liken", "lilac", "limbo", "limit", "linen", "liner", "lingo", "lipid", "lithe", "liver", "livid", "llama", "loamy", "loath", "lobby", "local", "locus", "lodge", "lofty",
        "logic", "login", "loopy", "loose", "lorry", "loser", "louse", "lousy", "lover", "lower", "lowly", "loyal", "lucid", "lucky", "lumen", "lumpy", "lunar", "lunch", "lunge", "lupus",
        "lurch", "lurid", "lusty", "lying", "lymph", "lyric", "macaw", "macho", "macro", "madam", "madly", "mafia", "magic", "magma", "maize", "major", "maker", "mambo", "mamma", "mammy",
        "manga", "mange", "mango", "mangy", "mania", "manic", "manly", "manor", "maple", "march", "marry", "marsh", "mason", "masse", "match", "matey", "mauve", "maxim", "maybe", "mayor",
        "mealy", "meant", "meaty", "mecca", "medal", "media", "medic", "melee", "melon", "mercy", "merge", "merit", "merry", "metal", "meter", "metro", "micro", "midge", "midst", "might",
        "milky", "mimic", "mince", "miner", "minim", "minor", "minty", "minus", "mirth", "miser", "missy", "mocha", "modal", "model", "modem", "mogul", "moist", "molar", "moldy", "money",
        "month", "moody", "moose", "moral", "moron", "morph", "mossy", "motel", "motif", "motor", "motto", "moult", "mound", "mount", "mourn", "mouse", "mouth", "mover", "movie", "mower",
        "mucky", "mucus", "muddy", "mulch", "mummy", "munch", "mural", "murky", "mushy", "music", "musky", "musty", "myrrh", "nadir", "naive", "nanny", "nasal", "nasty", "natal", "naval",
        "navel", "needy", "neigh", "nerdy", "nerve", "never", "newer", "newly", "nicer", "niche", "niece", "night", "ninja", "ninny", "ninth", "noble", "nobly", "noise", "noisy", "nomad",
        "noose", "north", "nosey", "notch", "novel", "nudge", "nurse", "nutty", "nylon", "nymph", "oaken", "obese", "occur", "ocean", "octal", "octet", "odder", "oddly", "offal", "offer",
        "often", "olden", "older", "olive", "ombre", "omega", "onion", "onset", "opera", "opine", "opium", "optic", "orbit", "order", "organ", "other", "otter", "ought", "ounce", "outdo",
        "outer", "outgo", "ovary", "ovate", "overt", "ovine", "ovoid", "owing", "owner", "oxide", "ozone", "paddy", "pagan", "paint", "paler", "palsy", "panel", "panic", "pansy", "papal",
        "paper", "parer", "parka", "parry", "parse", "party", "pasta", "paste", "pasty", "patch", "patio", "patsy", "patty", "pause", "payee", "payer", "peace", "peach", "pearl", "pecan",
        "pedal", "penal", "pence", "penne", "penny", "perch", "peril", "perky", "pesky", "pesto", "petal", "phase", "phone", "phony", "photo", "piano", "picky", "piece", "piety", "piggy",
        "pilot", "pinch", "piney", "pinky", "pinto", "piper", "pique", "pitch", "pithy", "pivot", "pixel", "pixie", "pizza", "place", "plaid", "plain", "plait", "plane", "plank", "plant",
        "plate", "plaza", "plead", "pleat", "plied", "plier", "pluck", "plumb", "plume", "plump", "plunk", "plush", "poesy", "point", "poise", "poker", "polar", "polka", "polyp", "pooch",
        "poppy", "porch", "poser", "posit", "posse", "pouch", "pound", "pouty", "power", "prank", "prawn", "preen", "press", "price", "prick", "pride", "pried", "prime", "primo", "print",
        "prior", "prism", "privy", "prize", "probe", "prone", "prong", "proof", "prose", "proud", "prove", "prowl", "proxy", "prude", "prune", "psalm", "pubic", "pudgy", "puffy", "pulpy",
        "pulse", "punch", "pupil", "puppy", "puree", "purer", "purge", "purse", "pushy", "putty", "pygmy", "quack", "quail", "quake", "qualm", "quark", "quart", "quash", "quasi", "queen",
        "queer", "quell", "query", "quest", "queue", "quick", "quiet", "quill", "quilt", "quirk", "quite", "quota", "quote", "quoth", "rabbi", "rabid", "racer", "radar", "radii", "radio",
        "rainy", "raise", "rajah", "rally", "ralph", "ramen", "ranch", "randy", "range", "rapid", "rarer", "raspy", "ratio", "ratty", "raven", "rayon", "razor", "reach", "react", "ready",
        "realm", "rearm", "rebar", "rebel", "rebus", "rebut", "recap", "recur", "recut", "reddy", "redeem", "redux", "reedy", "refer", "refit", "regal", "rehab", "reign", "relax", "relay",
        "relic", "remit", "renal", "renew", "repay", "repel", "reply", "rerun", "reset", "resin", "retch", "retro", "retry", "reuse", "revel", "revue", "rhino", "rhyme", "rider", "ridge",
        "rifle", "right", "rigid", "rigor", "rinse", "ripen", "riper", "risen", "riser", "risky", "rival", "river", "rivet", "roach", "roast", "robin", "robot", "rocky", "rodeo", "roger",
        "rogue", "roomy", "roost", "rotor", "rouge", "rough", "round", "rouse", "route", "rover", "rowdy", "rower", "royal", "ruddy", "ruder", "rugby", "ruler", "rumba", "rumor", "rupee",
        "rural", "rusty", "sadly", "safer", "saint", "salad", "sally", "salon", "salsa", "salty", "salve", "salvo", "sandy", "saner", "sappy", "sassy", "satin", "satyr", "sauce", "saucy",
        "sauna", "saute", "savor", "savoy", "savvy", "scald", "scale", "scalp", "scaly", "scamp", "scant", "scape", "scare", "scarf", "scary", "scene", "scent", "scion", "scoff", "scold",
        "scone", "scoop", "scoot", "scope", "score", "scorn", "scour", "scout", "scowl", "scram", "scrap", "scree", "screw", "scrub", "scrum", "scuba", "sedan", "seedy", "segue", "seize",
        "semen", "sense", "sepia", "serif", "serum", "serve", "setup", "seven", "sever", "sewer", "shack", "shade", "shady", "shaft", "shake", "shaky", "shale", "shall", "shalt", "shame",
        "shank", "shape", "shard", "share", "shark", "sharp", "shave", "shawl", "shear", "sheen", "sheep", "sheer", "sheet", "sheik", "shelf", "shell", "shied", "shift", "shine", "shiny",
        "shire", "shirk", "shirt", "shoal", "shock", "shone", "shook", "shoot", "shore", "shorn", "short", "shout", "shove", "shown", "showy", "shrew", "shrub", "shrug", "shuck", "shunt",
        "shush", "shyly", "siege", "sieve", "sight", "sigma", "silky", "silly", "since", "sinew", "singe", "siren", "sissy", "sixth", "sixty", "skate", "skier", "skiff", "skill", "skimp",
        "skirt", "skulk", "skull", "skunk", "slack", "slain", "slang", "slant", "slash", "slate", "slave", "sleek", "sleep", "sleet", "slept", "slice", "slick", "slide", "slime", "slimy",
        "sling", "slink", "sloop", "slope", "slosh", "sloth", "slump", "slung", "slunk", "slurp", "slush", "slyly", "smack", "small", "smart", "smash", "smear", "smell", "smelt", "smile",
        "smirk", "smite", "smith", "smock", "smoke", "smoky", "smote", "snack", "snail", "snake", "snaky", "snare", "snarl", "sneak", "sneer", "snide", "sniff", "snipe", "snoop", "snore",
        "snort", "snout", "snowy", "snuck", "snuff", "soapy", "sober", "soggy", "solar", "solid", "solve", "sonar", "sonic", "sooth", "sooty", "sorry", "sound", "south", "sower", "space",
        "spade", "spank", "spare", "spark", "spasm", "spawn", "speak", "spear", "speck", "speed", "spell", "spelt", "spend", "spent", "sperm", "spice", "spicy", "spied", "spiel", "spike",
        "spiky", "spill", "spilt", "spine", "spiny", "spire", "spite", "splat", "split", "spoil", "spoke", "spoof", "spook", "spool", "spoon", "spore", "sport", "spout", "spray", "spree",
        "sprig", "spunk", "spurn", "spurt", "squad", "squat", "squib", "stack", "staff", "stage", "staid", "stain", "stair", "stake", "stale", "stalk", "stall", "stamp", "stand", "stank",
        "stare", "stark", "start", "stash", "state", "stave", "stead", "steak", "steal", "steam", "steed", "steel", "steep", "steer", "stein", "stern", "stick", "stiff", "still", "stilt",
        "sting", "stink", "stint", "stock", "stoic", "stoke", "stole", "stomp", "stone", "stony", "stood", "stool", "stoop", "store", "stork", "storm", "story", "stout", "stove", "strap",
        "straw", "stray", "strip", "strut", "stuck", "study", "stuff", "stump", "stung", "stunk", "stunt", "style", "suave", "sugar", "suing", "suite", "sulky", "sully", "sumac", "sunny",
        "super", "surer", "surge", "surly", "sushi", "swami", "swamp", "swarm", "swash", "swath", "swear", "sweat", "sweep", "sweet", "swell", "swept", "swift", "swill", "swine", "swing",
        "swirl", "swish", "swoon", "swoop", "sword", "swore", "sworn", "swung", "synod", "syrup", "tabby", "table", "taboo", "tacit", "tacky", "taffy", "taint", "taken", "taker", "tally", "tares",
        "talon", "tamer", "tango", "tangy", "taper", "tapir", "tardy", "tarot", "taste", "tasty", "tatty", "taunt", "tawny", "teach", "teary", "tease", "teddy", "teeth", "tempo", "tenet",
        "tenor", "tense", "tenth", "tepee", "tepid", "terra", "terse", "testy", "thank", "theft", "their", "theme", "there", "these", "theta", "thick", "thief", "thigh", "thing", "think",
        "third", "thong", "thorn", "those", "three", "threw", "throb", "throw", "thrum", "thumb", "thump", "thyme", "tiara", "tibia", "tidal", "tiger", "tight", "tilde", "timer", "timid",
        "tipsy", "titan", "tithe", "title", "toast", "today", "toddy", "token", "tonal", "tonga", "tonic", "tooth", "topaz", "topic", "torch", "torso", "torus", "total", "totem", "touch",
        "tough", "towel", "tower", "toxic", "toxin", "trace", "track", "tract", "trade", "trail", "train", "trait", "tramp", "trash", "trawl", "tread", "treat", "trend", "triad", "trial",
        "tribe", "trice", "trick", "tried", "tripe", "trite", "troll", "troop", "trope", "trout", "trove", "truce", "truck", "truer", "truly", "trump", "trunk", "truss", "trust", "truth",
        "tryst", "tubal", "tuber", "tulip", "tulle", "tummy", "tumor", "tunic", "turbo", "tutor", "twang", "tweak", "tweed", "tweet", "twice", "twine", "twirl", "twist", "twixt", "tying",
        "udder", "ulcer", "ultra", "umbra", "uncle", "uncut", "under", "undid", "undue", "unfed", "unfit", "unify", "union", "unite", "unity", "unlit", "unmet", "unset", "untie", "until",
        "unwed", "unzip", "upper", "upset", "urban", "urine", "usage", "usher", "using", "usual", "usurp", "utile", "utter", "vague", "valet", "valid", "valor", "value", "valve", "vampy",
        "vapor", "vault", "vaunt", "vegan", "venom", "venue", "verge", "verse", "verso", "verve", "vicar", "video", "vigil", "vigor", "villa", "vinyl", "viola", "viper", "viral", "virus",
        "visit", "visor", "vista", "vital", "vivid", "vixen", "vocal", "vodka", "vogue", "voice", "voila", "vomit", "voter", "vouch", "vowel", "vying", "wacky", "wader", "wafer", "wager",
        "wagon", "waist", "waive", "waltz", "warty", "waste", "watch", "water", "waver", "waxen", "weary", "weave", "wedge", "weedy", "weigh", "weird", "welch", "welsh", "whack", "whale",
        "wharf", "wheat", "wheel", "whelp", "where", "which", "whiff", "while", "whine", "whiny", "whirl", "whisk", "white", "whole", "whoop", "whose", "widen", "wider", "widow", "width",
        "wield", "wight", "willy", "wimpy", "wince", "winch", "windy", "wiser", "wispy", "witch", "witty", "woken", "woman", "women", "woody", "wooer", "wooly", "woozy", "wordy", "world",
        "worry", "worse", "worst", "worth", "would", "wound", "woven", "wrack", "wrath", "wreak", "wreck", "wrest", "wring", "wrist", "write", "wrong", "wrote", "wrung", "wryly", "yacht",
        "yearn", "yeast", "yield", "young", "youth", "zebra", "zesty", "zonal"
    ];
    
    const gameBoard = document.getElementById('game-board');
    const submitButton = document.getElementById('submit-button');
    const restartButton = document.getElementById('restart-button');
    const gameOverContainer = document.getElementById('game-over-container');
    const gameOverTitle = document.getElementById('game-over-title');
    const gameOverText = document.getElementById('game-over-text');
    const statusMessage = document.getElementById('status-message');
    const feedbackInput = document.getElementById('feedback-input');
    const actionArea = document.getElementById('action-area');
    const addWordContainer = document.getElementById('add-word-container');
    const addWordInput = document.getElementById('add-word-input');
    const addWordButton = document.getElementById('add-word-button');
    const confettiCanvas = document.getElementById('confetti-canvas');
    const confettiCtx = confettiCanvas.getContext('2d');
    const difficultyNormalBtn = document.getElementById('difficulty-normal');
    const difficultyHardBtn = document.getElementById('difficulty-hard');
    const shareButton = document.getElementById('share-button');

    let state = {};
    let confettiParticles = [];
    const colorMap = { g: 'correct', y: 'present', b: 'absent' };
    const colorClasses = { absent: 'bg-absent', present: 'bg-present', correct: 'bg-correct' };

    let sounds;
    const initSounds = () => {
        if (sounds) return;
        sounds = {
            flip: new Tone.Synth({ oscillator: { type: 'sine' }, envelope: { attack: 0.005, decay: 0.1, sustain: 0.3, release: 0.1 } }).toDestination(),
            win: new Tone.PolySynth(Tone.Synth, { oscillator: { type: 'fatsawtooth' }, envelope: { attack: 0.01, decay: 0.2, sustain: 0.2, release: 0.2 } }).toDestination(),
            lose: new Tone.PolySynth(Tone.Synth, { oscillator: { type: 'fatsquare' }, envelope: { attack: 0.01, decay: 0.5, sustain: 0.1, release: 0.5 } }).toDestination(),
            invalid: new Tone.Synth({ oscillator: { type: 'triangle' }, envelope: { attack: 0.01, decay: 0.2, sustain: 0, release: 0.2 } }).toDestination(),
        };
    };
    document.body.addEventListener('click', async () => {
        await Tone.start();
        initSounds();
    }, { once: true });


    function resetState() {
        state = {
            possibleWords: [...wordList],
            guesses: [],
            absentLetters: new Set(),
            presentLetters: new Set(),
            correctLetters: Array(5).fill(null),
            yellowPositions: {},
            isGameOver: false,
            isAnimating: false,
            guessCount: 0,
            currentGuessWord: '',
            difficulty: 'normal'
        };
    }

    function startGame() {
        resetState();
        state.difficulty = difficultyNormalBtn.classList.contains('bg-indigo-500') ? 'normal' : 'hard';
        gameBoard.innerHTML = '';
        for(let i = 0; i < 6; i++) {
            const row = document.createElement('div');
            row.className = 'flex justify-center gap-1.5';
            row.id = `row-${i}`;
            for(let j=0; j<5; j++) {
                const tileContainer = document.createElement('div');
                tileContainer.className = 'tile-container w-14 h-14 sm:w-16 sm:h-16 bg-gray-200 rounded-md shadow-inner';
                row.appendChild(tileContainer);
            }
            gameBoard.appendChild(row);
        }
        
        actionArea.classList.remove('hidden');
        gameOverContainer.classList.add('hidden');
        statusMessage.textContent = '';
        statusMessage.className = 'text-center h-12 flex items-center justify-center font-semibold text-xl mt-4';
        submitButton.disabled = true;
        feedbackInput.value = '';
        feedbackInput.disabled = true;
        
        setTimeout(() => {
            makeGuess('crane');
        }, 500);
    }

    function makeGuess(word) {
        state.currentGuessWord = word;
        const row = document.getElementById(`row-${state.guessCount}`);
        row.innerHTML = ''; 

        word.split('').forEach((letter) => {
            const tileContainer = document.createElement('div');
            tileContainer.className = 'tile-container';
            const tile = document.createElement('div');
            tile.className = 'tile';
            const front = document.createElement('div');
            front.className = 'tile-front';
            front.textContent = letter.toUpperCase();
            const back = document.createElement('div');
            back.className = 'tile-back';
            back.textContent = letter.toUpperCase();
            tile.appendChild(front);
            tile.appendChild(back);
            tileContainer.appendChild(tile);
            row.appendChild(tileContainer);
        });
        
        submitButton.disabled = false;
        feedbackInput.disabled = false;
        feedbackInput.focus();
    }

    async function handleFeedback() {
        if (state.isGameOver || state.isAnimating) return;
        
        const feedbackStr = feedbackInput.value.toLowerCase();
        if (!/^[gyb]{5}$/.test(feedbackStr)) {
            setStatusMessage('Please enter a 5-letter pattern using G, Y, or B.', 'shake', 'text-red-600');
            sounds?.invalid.triggerAttackRelease("C3", "0.2");
            return;
        }

        const feedback = feedbackStr.split('').map(char => colorMap[char]);
        state.guesses.push(feedback);
        
        if (!validateFeedback(state.currentGuessWord, feedback)) {
             setStatusMessage('Contradictory feedback. (e.g., a letter is Green but also Grey)', 'shake', 'text-red-600');
             sounds?.invalid.triggerAttackRelease("C3", "0.2");
             state.guesses.pop();
             return;
        }

        state.isAnimating = true;
        submitButton.disabled = true;
        feedbackInput.disabled = true;
        statusMessage.textContent = '';

        const currentTiles = document.getElementById(`row-${state.guessCount}`).querySelectorAll('.tile');
        for (let i = 0; i < currentTiles.length; i++) {
            const tile = currentTiles[i];
            const backFace = tile.querySelector('.tile-back');
            backFace.classList.remove(...Object.values(colorClasses));
            backFace.classList.add(colorClasses[feedback[i]]);
            await new Promise(resolve => setTimeout(resolve, 250));
            tile.classList.add('is-flipped');
            sounds?.flip.triggerAttackRelease("C5", "0.1");
        }
        await new Promise(resolve => setTimeout(resolve, 700));

        state.guessCount++;

        if (feedback.every(f => f === 'correct')) {
            winGame(currentTiles);
            return;
        }

        if (state.guessCount === 6) {
            endGameNoSolution("I've run out of guesses!");
            return;
        }

        processFeedback(state.currentGuessWord, feedback);
        generateNextGuess();
        
        feedbackInput.value = '';
        state.isAnimating = false;
    }
    
    function validateFeedback(word, feedback) {
        const knownCorrect = new Set();
        const knownPresent = new Set();
        for(let i=0; i<5; i++) {
            if(feedback[i] === 'correct') knownCorrect.add(word[i]);
            if(feedback[i] === 'present') knownPresent.add(word[i]);
        }
        for(let i=0; i<5; i++) {
            if(feedback[i] === 'absent' && (knownCorrect.has(word[i]) || knownPresent.has(word[i]))) {
                return false;
            }
        }
        return true;
    }

    function processFeedback(word, feedback) {
         for (let i = 0; i < 5; i++) {
            const letter = word[i];
            const result = feedback[i];
            if (result === 'correct') {
                state.correctLetters[i] = letter;
                state.presentLetters.add(letter);
            } else if (result === 'present') {
                state.presentLetters.add(letter);
                if (!state.yellowPositions[letter]) state.yellowPositions[letter] = [];
                state.yellowPositions[letter].push(i);
            } else if (result === 'absent') {
                if (!state.presentLetters.has(letter) && !state.correctLetters.includes(letter)) {
                    state.absentLetters.add(letter);
                }
            }
        }
    }

    function generateNextGuess() {
        setStatusMessage('Thinking...', '', 'text-gray-500');
        
        setTimeout(() => {
            state.possibleWords = state.possibleWords.filter(word => {
                return checkWordAgainstClues(word, state);
            });
            
            setStatusMessage(`${state.possibleWords.length} words remaining...`, '', 'text-gray-500');

            let bestGuess = '';
            if (state.possibleWords.length === 0) {
                endGameNoSolution("I'm stumped! My dictionary might not have your word.");
                return;
            } else if (state.possibleWords.length <= 2 || state.difficulty === 'normal') {
                bestGuess = state.possibleWords[0];
            } else {
                bestGuess = findOptimalGuess();
            }
            
            setTimeout(() => {
                statusMessage.textContent = '';
                makeGuess(bestGuess);
            }, 800)
        }, 700);
    }

    function findOptimalGuess() {
        const letterFrequency = {};
        for(const word of state.possibleWords) {
            const uniqueLetters = new Set(word.split(''));
            for(const letter of uniqueLetters) {
                letterFrequency[letter] = (letterFrequency[letter] || 0) + 1;
            }
        }
        let bestScore = -1;
        let bestWord = state.possibleWords[0];
        for(const word of wordList) {
            const uniqueLetters = new Set(word.split(''));
            let score = 0;
            for(const letter of uniqueLetters) {
                if(!state.presentLetters.has(letter) && !state.absentLetters.has(letter) && !state.correctLetters.includes(letter)) {
                   score += (letterFrequency[letter] || 0);
                }
            }
            if (score > bestScore) {
                bestScore = score;
                bestWord = word;
            }
        }
        return bestWord;
    }

    function checkWordAgainstClues(word, currentState) {
        for (const letter of currentState.absentLetters) {
            if (word.includes(letter)) return false;
        }
        for (const letter of currentState.presentLetters) {
            if (!word.includes(letter)) return false;
        }
        for (let i = 0; i < 5; i++) {
            if (currentState.correctLetters[i] && word[i] !== currentState.correctLetters[i]) {
                return false;
            }
        }
        for (const letter in currentState.yellowPositions) {
            for (const pos of currentState.yellowPositions[letter]) {
                if (word[pos] === letter) return false;
            }
        }
        return true;
    }

    function setStatusMessage(msg, animationClass = '', textClass = '') {
        statusMessage.textContent = msg;
        statusMessage.className = `text-center h-12 flex items-center justify-center font-semibold text-lg ${textClass}`;
        if (animationClass) {
            const elementToShake = addWordContainer.classList.contains('hidden') ? feedbackInput : addWordInput;
            elementToShake.classList.add(animationClass);
            elementToShake.classList.add('border-red-500', 'ring-red-500');
            setTimeout(() => {
                elementToShake.classList.remove('shake', 'border-red-500', 'ring-red-500');
            }, 600);
        }
    }

    async function winGame(tiles) {
        endGame();
        gameOverTitle.textContent = 'I Won!';
        gameOverText.textContent = `I guessed your word "${state.currentGuessWord.toUpperCase()}" in ${state.guessCount} tries.`;
        addWordContainer.classList.add('hidden');
        sounds?.win.triggerAttackRelease(["C4", "E4", "G4", "C5"], 0.4);
        launchConfetti();
        for (let i = 0; i < tiles.length; i++) {
            await new Promise(resolve => setTimeout(resolve, 80));
            tiles[i].classList.add('jump');
        }
    }

    function endGameNoSolution(message) {
        endGame();
        gameOverTitle.textContent = 'You Stumped Me!';
        gameOverText.textContent = message;
        addWordContainer.classList.remove('hidden');
        sounds?.lose.triggerAttackRelease(["C4", "A3", "F3", "D3"], 0.8);
    }

    function handleAddWord() {
        const userWord = addWordInput.value.toLowerCase();
        if (userWord.length !== 5) {
            setStatusMessage("The word must be 5 letters long.", 'shake', 'text-red-600');
            return;
        }

        if (checkWordAgainstClues(userWord, state)) {
            wordList.push(userWord);
            gameOverText.textContent = "Thanks for teaching me! My dictionary is updated for next time.";
            addWordContainer.classList.add('hidden');
        } else {
            setStatusMessage("That word doesn't match the clues you gave.", 'shake', 'text-red-600');
        }
    }

    function endGame() {
        state.isGameOver = true;
        state.isAnimating = true;
        actionArea.classList.add('hidden');
        statusMessage.classList.add('hidden');
        gameOverContainer.classList.remove('hidden');
    }

    function launchConfetti() {
        confettiCanvas.width = window.innerWidth;
        confettiCanvas.height = window.innerHeight;
        const confettiCount = 200;
        const colors = ['#6aaa64', '#c9b458', '#787c7e', '#ffffff', '#4f46e5'];
        confettiParticles = [];
        for (let i = 0; i < confettiCount; i++) {
            confettiParticles.push({
                x: Math.random() * confettiCanvas.width,
                y: -Math.random() * confettiCanvas.height,
                size: Math.random() * 5 + 2,
                color: colors[Math.floor(Math.random() * colors.length)],
                speedX: Math.random() * 3 - 1.5,
                speedY: Math.random() * 5 + 2,
                angle: Math.random() * 360,
                spin: (Math.random() - 0.5) * 10,
            });
        }
        animateConfetti();
    }

    function animateConfetti() {
        confettiCtx.clearRect(0, 0, confettiCanvas.width, confettiCanvas.height);
        let stillAnimating = false;
        confettiParticles.forEach(p => {
            p.x += p.speedX;
            p.y += p.speedY;
            p.angle += p.spin;
            p.speedY += 0.05;
            
            if (p.y < confettiCanvas.height) {
                stillAnimating = true;
                confettiCtx.save();
                confettiCtx.translate(p.x, p.y);
                confettiCtx.rotate(p.angle * Math.PI / 180);
                confettiCtx.fillStyle = p.color;
                confettiCtx.fillRect(-p.size / 2, -p.size / 2, p.size, p.size);
                confettiCtx.restore();
            }
        });
        if (stillAnimating) {
            requestAnimationFrame(animateConfetti);
        } else {
             confettiCtx.clearRect(0, 0, confettiCanvas.width, confettiCanvas.height);
        }
    }
    
    function handleShare() {
        const emojiMap = { correct: '🟩', present: '🟨', absent: '⬛' };
        let shareText = `AI Wordle Guesser ${state.guessCount}/6 (${state.difficulty.charAt(0).toUpperCase() + state.difficulty.slice(1)})\n\n`;
        state.guesses.forEach(guess => {
            shareText += guess.map(result => emojiMap[result]).join('') + '\n';
        });

        const textArea = document.createElement('textarea');
        textArea.value = shareText;
        document.body.appendChild(textArea);
        textArea.select();
        try {
            document.execCommand('copy');
            const originalText = shareButton.textContent;
            shareButton.textContent = 'Copied!';
            setTimeout(() => {
                shareButton.textContent = originalText;
            }, 2000);
        } catch (err) {
            gameOverText.textContent = 'Could not copy results.';
        }
        document.body.removeChild(textArea);
    }

    submitButton.addEventListener('click', handleFeedback);
    restartButton.addEventListener('click', startGame);
    addWordButton.addEventListener('click', handleAddWord);
    shareButton.addEventListener('click', handleShare);
    
    difficultyNormalBtn.addEventListener('click', () => {
        difficultyNormalBtn.classList.add('ring-2', 'ring-indigo-500', 'bg-indigo-500', 'text-white');
        difficultyHardBtn.classList.remove('ring-2', 'ring-indigo-500', 'bg-indigo-500', 'text-white');
        difficultyHardBtn.classList.add('bg-gray-200', 'text-gray-700');
        startGame();
    });
    difficultyHardBtn.addEventListener('click', () => {
        difficultyHardBtn.classList.add('ring-2', 'ring-indigo-500', 'bg-indigo-500', 'text-white');
        difficultyNormalBtn.classList.remove('ring-2', 'ring-indigo-500', 'bg-indigo-500', 'text-white');
        difficultyNormalBtn.classList.add('bg-gray-200', 'text-gray-700');
        startGame();
    });

    feedbackInput.addEventListener('keyup', (e) => {
        if (e.key === 'Enter') {
            handleFeedback();
        }
    });
    addWordInput.addEventListener('keyup', (e) => {
        if (e.key === 'Enter') {
            handleAddWord();
        }
    });

    startGame();
});