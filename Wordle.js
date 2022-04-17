// CHECK GDOCS LISTS

var allWords = [];
let greenLetters = new Map();
let yellowLetters = new Map();
let blackLetters = new Map();
let eligibleWords = new Map();
let allWordFreq = new Map();
var words = [];
let letters = [];
let chars = [];
var wordGuess = "";
var colorResult = "";
var wordCounter = 0;
var theAnswer = "";
let vals = [];
var results = 0;
var reportle = 0;
var reportleMultiplier = 1;
var prevArrSize = 12967;
var allLetterFrequencyUser = 0;
var allPositionUser = 0;
var allWordFrequencyUser = 0;
var allLetterFrequencyComp = 0;
var allPositionComp = 0;
var allWordFrequencyComp = 0;
var startingWord = false;
var inLoop = false;

const storAllWords = [
	"cigar", "rebut", "sissy", "humph", "awake", "blush", "focal", "evade", "naval",
	"serve", "heath", "dwarf", "model", "karma", "stink", "grade", "quiet", "bench", "abate", "feign", "major",
	"death", "fresh", "crust", "stool", "colon", "abase", "marry", "react", "batty", "pride", "floss", "helix",
	"croak", "staff", "paper", "unfed", "whelp", "trawl", "outdo", "adobe", "crazy", "sower", "repay", "digit",
	"crate", "cluck", "spike", "mimic", "pound", "maxim", "linen", "unmet", "flesh", "booby", "forth", "first",
	"stand", "belly", "ivory", "seedy", "print", "yearn", "drain", "bribe", "stout", "panel", "crass", "flume",
	"offal", "agree", "error", "swirl", "argue", "bleed", "delta", "flick", "totem", "wooer", "front", "shrub",
	"parry", "biome", "lapel", "start", "greet", "goner", "golem", "lusty", "loopy", "round", "audit", "lying",
	"gamma", "labor", "islet", "civic", "forge", "corny", "moult", "basic", "salad", "agate", "spicy", "spray",
	"essay", "fjord", "spend", "kebab", "guild", "aback", "motor", "alone", "hatch", "hyper", "thumb", "dowry",
	"ought", "belch", "dutch", "pilot", "tweed", "comet", "jaunt", "enema", "steed", "abyss", "growl", "fling",
	"dozen", "boozy", "erode", "world", "gouge", "click", "briar", "great", "altar", "pulpy", "blurt", "coast",
	"duchy", "groin", "fixer", "group", "rogue", "badly", "smart", "pithy", "gaudy", "chill", "heron", "vodka",
	"finer", "surer", "radio", "rouge", "perch", "retch", "wrote", "clock", "tilde", "store", "prove", "bring",
	"solve", "cheat", "grime", "exult", "usher", "epoch", "triad", "break", "rhino", "viral", "conic", "masse",
	"sonic", "vital", "trace", "using", "peach", "champ", "baton", "brake", "pluck", "craze", "gripe", "weary",
	"picky", "acute", "ferry", "aside", "tapir", "troll", "unify", "rebus", "boost", "truss", "siege", "tiger",
	"banal", "slump", "crank", "gorge", "query", "drink", "favor", "abbey", "tangy", "panic", "solar", "shire",
	"proxy", "point", "robot", "prick", "wince", "crimp", "knoll", "sugar", "whack", "mount", "perky", "could",
	"wrung", "light", "those", "moist", "shard", "pleat", "aloft", "skill", "elder", "frame", "humor", "pause",
	"ulcer", "ultra", "robin", "cynic", "aroma", "caulk", "shake", "dodge", "swill", "tacit", "other", "thorn",
	"trove", "bloke", "vivid", "spill", "chant", "choke", "rupee", "nasty", "mourn", "ahead", "brine", "cloth",
	"hoard", "sweet", "month", "lapse", "watch", "today", "focus", "smelt", "tease", "cater", "movie", "saute",
	"allow", "renew", "their", "slosh", "purge", "chest", "depot", "epoxy", "nymph", "found", "shall", "stove",
	"lowly", "snout", "trope", "fewer", "shawl", "natal", "comma", "foray", "scare", "stair", "black", "squad",
	"royal", "chunk", "mince", "shame", "cheek", "ample", "flair", "foyer", "cargo", "oxide", "plant", "olive",
	"inert", "askew", "heist", "shown", "zesty", "trash", "larva", "forgo", "story", "hairy", "train", "homer",
	"badge", "midst", "canny", "fetus", "butch", "farce", "slung", "tipsy", "metal", "yield", "delve", "being",
	"scour", "glass", "gamer", "scrap", "money", "hinge", "album", "vouch", "asset", "tiara", "crept", "bayou",
	"atoll", "manor", "creak", "showy", "phase", "froth", "depth", "gloom", "flood", "trait", "girth", "piety",
	"goose", "float", "donor", "atone", "primo", "apron", "blown", "cacao", "loser", "input", "gloat", "awful",
	"brink", "smite", "beady", "rusty", "retro", "droll", "gawky", "hutch", "pinto", "egret", "lilac", "sever",
	"field", "fluff", "flack", "agape", "voice", "stead", "stalk", "berth", "madam", "night", "bland", "liver",
	"wedge", "augur", "roomy", "wacky", "flock", "angry", "trite", "aphid", "tryst", "midge", "power", "elope",
	"cinch", "motto", "stomp", "upset", "bluff", "cramp", "quart", "coyly", "youth", "rhyme", "buggy", "alien",
	"smear", "unfit", "patty", "cling", "glean", "label", "hunky", "khaki", "poker", "gruel", "twice", "twang",
	"shrug", "treat", "waste", "merit", "woven", "needy", "clown", "widow", "irony", "ruder", "gauze", "chief",
	"onset", "prize", "fungi", "charm", "gully", "inter", "whoop", "taunt", "leery", "class", "theme", "lofty",
	"tibia", "booze", "alpha", "thyme", "doubt", "parer", "chute", "stick", "trice", "alike", "recap", "saint",
	"glory", "grate", "admit", "brisk", "soggy", "usurp", "scald", "scorn", "leave", "twine", "sting", "bough",
	"marsh", "sloth", "dandy", "vigor", "howdy", "enjoy", "valid", "ionic", "equal", "floor", "catch", "spade",
	"stein", "exist", "quirk", "denim", "grove", "spiel", "mummy", "fault", "foggy", "flout", "carry", "sneak",
	"libel", "waltz", "aptly", "piney", "inept", "aloud", "photo", "dream", "stale", "unite", "snarl", "baker",
	"there", "glyph", "pooch", "hippy", "spell", "folly", "louse", "gulch", "vault", "godly", "threw", "fleet",
	"grave", "inane", "shock", "crave", "spite", "valve", "skimp", "claim", "rainy", "musty", "pique", "daddy",
	"quasi", "arise", "aging", "valet", "opium", "avert", "stuck", "recut", "mulch", "genre", "plume", "rifle",
	"count", "incur", "total", "wrest", "mocha", "deter", "study", "lover", "safer", "rivet", "funny", "smoke",
	"mound", "undue", "sedan", "pagan", "swine", "guile", "gusty", "equip", "tough", "canoe", "chaos", "covet",
	"human", "udder", "lunch", "blast", "stray", "manga", "melee", "lefty", "quick", "paste", "given", "octet",
	"risen", "groan", "leaky", "grind", "carve", "loose", "sadly", "spilt", "apple", "slack", "honey", "final",
	"sheen", "eerie", "minty", "slick", "derby", "wharf", "spelt", "coach", "erupt", "singe", "price", "spawn",
	"fairy", "jiffy", "filmy", "stack", "chose", "sleep", "ardor", "nanny", "niece", "woozy", "handy", "grace",
	"ditto", "stank", "cream", "usual", "diode", "valor", "angle", "ninja", "muddy", "chase", "reply", "prone",
	"spoil", "heart", "shade", "diner", "arson", "onion", "sleet", "dowel", "couch", "palsy", "bowel", "smile",
	"evoke", "creek", "lance", "eagle", "idiot", "siren", "built", "embed", "award", "dross", "annul", "goody",
	"frown", "patio", "laden", "humid", "elite", "lymph", "edify", "might", "reset", "visit", "gusto", "purse",
	"vapor", "crock", "write", "sunny", "loath", "chaff", "slide", "queer", "venom", "stamp", "sorry", "still",
	"acorn", "aping", "pushy", "tamer", "hater", "mania", "awoke", "brawn", "swift", "exile", "birch", "lucky",
	"freer", "risky", "ghost", "plier", "lunar", "winch", "snare", "nurse", "house", "borax", "nicer", "lurch",
	"exalt", "about", "savvy", "toxin", "tunic", "pried", "inlay", "chump", "lanky", "cress", "eater", "elude",
	"cycle", "kitty", "boule", "moron", "tenet", "place", "lobby", "plush", "vigil", "index", "blink", "clung",
	"qualm", "croup", "clink", "juicy", "stage", "decay", "nerve", "flier", "shaft", "crook", "clean", "china",
	"ridge", "vowel", "gnome", "snuck", "icing", "spiny", "rigor", "snail", "flown", "rabid", "prose", "thank",
	"poppy", "budge", "fiber", "moldy", "dowdy", "kneel", "track", "caddy", "quell", "dumpy", "paler", "swore",
	"rebar", "scuba", "splat", "flyer", "horny", "mason", "doing", "ozone", "amply", "molar", "ovary", "beset",
	"queue", "cliff", "magic", "truce", "sport", "fritz", "edict", "twirl", "verse", "llama", "eaten", "range",
	"whisk", "hovel", "rehab", "macaw", "sigma", "spout", "verve", "sushi", "dying", "fetid", "brain", "buddy",
	"thump", "scion", "candy", "chord", "basin", "march", "crowd", "arbor", "gayly", "musky", "stain", "dally",
	"bless", "bravo", "stung", "title", "ruler", "kiosk", "blond", "ennui", "layer", "fluid", "tatty", "score",
	"cutie", "zebra", "barge", "matey", "bluer", "aider", "shook", "river", "privy", "betel", "frisk", "bongo",
	"begun", "azure", "weave", "genie", "sound", "glove", "braid", "scope", "wryly", "rover", "assay", "ocean",
	"bloom", "irate", "later", "woken", "silky", "wreck", "dwelt", "slate", "smack", "solid", "amaze", "hazel",
	"wrist", "jolly", "globe", "flint", "rouse", "civil", "vista", "relax", "cover", "alive", "beech", "jetty",
	"bliss", "vocal", "often", "dolly", "eight", "joker", "since", "event", "ensue", "shunt", "diver", "poser",
	"worst", "sweep", "alley", "creed", "anime", "leafy", "bosom", "dunce", "stare", "pudgy", "waive", "choir",
	"stood", "spoke", "outgo", "delay", "bilge", "ideal", "clasp", "seize", "hotly", "laugh", "sieve", "block",
	"meant", "grape", "noose", "hardy", "shied", "drawl", "daisy", "putty", "strut", "burnt", "tulip", "crick",
	"idyll", "vixen", "furor", "geeky", "cough", "naive", "shoal", "stork", "bathe", "aunty", "check", "prime",
	"brass", "outer", "furry", "razor", "elect", "evict", "imply", "demur", "quota", "haven", "cavil", "swear",
	"crump", "dough", "gavel", "wagon", "salon", "nudge", "harem", "pitch", "sworn", "pupil", "excel", "stony",
	"cabin", "unzip", "queen", "trout", "polyp", "earth", "storm", "until", "taper", "enter", "child", "adopt",
	"minor", "fatty", "husky", "brave", "filet", "slime", "glint", "tread", "steal", "regal", "guest", "every",
	"murky", "share", "spore", "hoist", "buxom", "inner", "otter", "dimly", "level", "sumac", "donut", "stilt",
	"arena", "sheet", "scrub", "fancy", "slimy", "pearl", "silly", "porch", "dingo", "sepia", "amble", "shady",
	"bread", "friar", "reign", "dairy", "quill", "cross", "brood", "tuber", "shear", "posit", "blank", "villa",
	"shank", "piggy", "freak", "which", "among", "fecal", "shell", "would", "algae", "large", "rabbi", "agony",
	"amuse", "bushy", "copse", "swoon", "knife", "pouch", "ascot", "plane", "crown", "urban", "snide", "relay",
	"abide", "viola", "rajah", "straw", "dilly", "crash", "amass", "third", "trick", "tutor", "woody", "blurb",
	"grief", "disco", "where", "sassy", "beach", "sauna", "comic", "clued", "creep", "caste", "graze", "snuff",
	"frock", "gonad", "drunk", "prong", "lurid", "steel", "halve", "buyer", "vinyl", "utile", "smell", "adage",
	"worry", "tasty", "local", "trade", "finch", "ashen", "modal", "gaunt", "clove", "enact", "adorn", "roast",
	"speck", "sheik", "missy", "grunt", "snoop", "party", "touch", "mafia", "emcee", "array", "south", "vapid",
	"jelly", "skulk", "angst", "tubal", "lower", "crest", "sweat", "cyber", "adore", "tardy", "swami", "notch",
	"groom", "roach", "hitch", "young", "align", "ready", "frond", "strap", "puree", "realm", "venue", "swarm",
	"offer", "seven", "dryer", "diary", "dryly", "drank", "acrid", "heady", "theta", "junto", "pixie", "quoth",
	"bonus", "shalt", "penne", "amend", "datum", "build", "piano", "shelf", "lodge", "suing", "rearm", "coral",
	"ramen", "worth", "psalm", "infer", "overt", "mayor", "ovoid", "glide", "usage", "poise", "randy", "chuck",
	"prank", "fishy", "tooth", "ether", "drove", "idler", "swath", "stint", "while", "begat", "apply", "slang",
	"tarot", "radar", "credo", "aware", "canon", "shift", "timer", "bylaw", "serum", "three", "steak", "iliac",
	"shirk", "blunt", "puppy", "penal", "joist", "bunny", "shape", "beget", "wheel", "adept", "stunt", "stole",
	"topaz", "chore", "fluke", "afoot", "bloat", "bully", "dense", "caper", "sneer", "boxer", "jumbo", "lunge",
	"space", "avail", "short", "slurp", "loyal", "flirt", "pizza", "conch", "tempo", "droop", "plate", "bible",
	"plunk", "afoul", "savoy", "steep", "agile", "stake", "dwell", "knave", "beard", "arose", "motif", "smash",
	"broil", "glare", "shove", "baggy", "mammy", "swamp", "along", "rugby", "wager", "quack", "squat", "snaky",
	"debit", "mange", "skate", "ninth", "joust", "tramp", "spurn", "medal", "micro", "rebel", "flank", "learn",
	"nadir", "maple", "comfy", "remit", "gruff", "ester", "least", "mogul", "fetch", "cause", "oaken", "aglow",
	"meaty", "gaffe", "shyly", "racer", "prowl", "thief", "stern", "poesy", "rocky", "tweet", "waist", "spire",
	"grope", "havoc", "patsy", "truly", "forty", "deity", "uncle", "swish", "giver", "preen", "bevel", "lemur",
	"draft", "slope", "annoy", "lingo", "bleak", "ditty", "curly", "cedar", "dirge", "grown", "horde", "drool",
	"shuck", "crypt", "cumin", "stock", "gravy", "locus", "wider", "breed", "quite", "chafe", "cache", "blimp",
	"deign", "fiend", "logic", "cheap", "elide", "rigid", "false", "renal", "pence", "rowdy", "shoot", "blaze",
	"envoy", "posse", "brief", "never", "abort", "mouse", "mucky", "sulky", "fiery", "media", "trunk", "yeast",
	"clear", "skunk", "scalp", "bitty", "cider", "koala", "duvet", "segue", "creme", "super", "grill", "after",
	"owner", "ember", "reach", "nobly", "empty", "speed", "gipsy", "recur", "smock", "dread", "merge", "burst",
	"kappa", "amity", "shaky", "hover", "carol", "snort", "synod", "faint", "haunt", "flour", "chair", "detox",
	"shrew", "tense", "plied", "quark", "burly", "novel", "waxen", "stoic", "jerky", "blitz", "beefy", "lyric",
	"hussy", "towel", "quilt", "below", "bingo", "wispy", "brash", "scone", "toast", "easel", "saucy", "value",
	"spice", "honor", "route", "sharp", "bawdy", "radii", "skull", "phony", "issue", "lager", "swell", "urine",
	"gassy", "trial", "flora", "upper", "latch", "wight", "brick", "retry", "holly", "decal", "grass", "shack",
	"dogma", "mover", "defer", "sober", "optic", "crier", "vying", "nomad", "flute", "hippo", "shark", "drier",
	"obese", "bugle", "tawny", "chalk", "feast", "ruddy", "pedal", "scarf", "cruel", "bleat", "tidal", "slush",
	"semen", "windy", "dusty", "sally", "igloo", "nerdy", "jewel", "shone", "whale", "hymen", "abuse", "fugue",
	"elbow", "crumb", "pansy", "welsh", "syrup", "terse", "suave", "gamut", "swung", "drake", "freed", "afire",
	"shirt", "grout", "oddly", "tithe", "plaid", "dummy", "broom", "blind", "torch", "enemy", "again", "tying",
	"pesky", "alter", "gazer", "noble", "ethos", "bride", "extol", "decor", "hobby", "beast", "idiom", "utter",
	"these", "sixth", "alarm", "erase", "elegy", "spunk", "piper", "scaly", "scold", "hefty", "chick", "sooty",
	"canal", "whiny", "slash", "quake", "joint", "swept", "prude", "heavy", "wield", "femme", "lasso", "maize",
	"shale", "screw", "spree", "smoky", "whiff", "scent", "glade", "spent", "prism", "stoke", "riper", "orbit",
	"cocoa", "guilt", "humus", "shush", "table", "smirk", "wrong", "noisy", "alert", "shiny", "elate", "resin",
	"whole", "hunch", "pixel", "polar", "hotel", "sword", "cleat", "mango", "rumba", "puffy", "filly", "billy",
	"leash", "clout", "dance", "ovate", "facet", "chili", "paint", "liner", "curio", "salty", "audio", "snake",
	"fable", "cloak", "navel", "spurt", "pesto", "balmy", "flash", "unwed", "early", "churn", "weedy", "stump",
	"lease", "witty", "wimpy", "spoof", "saner", "blend", "salsa", "thick", "warty", "manic", "blare", "squib",
	"spoon", "probe", "crepe", "knack", "force", "debut", "order", "haste", "teeth", "agent", "widen", "icily",
	"slice", "ingot", "clash", "juror", "blood", "abode", "throw", "unity", "pivot", "slept", "troop", "spare",
	"sewer", "parse", "morph", "cacti", "tacky", "spool", "demon", "moody", "annex", "begin", "fuzzy", "patch",
	"water", "lumpy", "admin", "omega", "limit", "tabby", "macho", "aisle", "skiff", "basis", "plank", "verge",
	"botch", "crawl", "lousy", "slain", "cubic", "raise", "wrack", "guide", "foist", "cameo", "under", "actor",
	"revue", "fraud", "harpy", "scoop", "climb", "refer", "olden", "clerk", "debar", "tally", "ethic", "cairn",
	"tulle", "ghoul", "hilly", "crude", "apart", "scale", "older", "plain", "sperm", "briny", "abbot", "rerun",
	"quest", "crisp", "bound", "befit", "drawn", "suite", "itchy", "cheer", "bagel", "guess", "broad", "axiom",
	"chard", "caput", "leant", "harsh", "curse", "proud", "swing", "opine", "taste", "lupus", "gumbo", "miner",
	"green", "chasm", "lipid", "topic", "armor", "brush", "crane", "mural", "abled", "habit", "bossy", "maker",
	"dusky", "dizzy", "lithe", "brook", "jazzy", "fifty", "sense", "giant", "surly", "legal", "fatal", "flunk",
	"began", "prune", "small", "slant", "scoff", "torus", "ninny", "covey", "viper", "taken", "moral", "vogue",
	"owing", "token", "entry", "booth", "voter", "chide", "elfin", "ebony", "neigh", "minim", "melon", "kneed",
	"decoy", "voila", "ankle", "arrow", "mushy", "tribe", "cease", "eager", "birth", "graph", "odder", "terra",
	"weird", "tried", "clack", "color", "rough", "weigh", "uncut", "ladle", "strip", "craft", "minus", "dicey",
	"titan", "lucid", "vicar", "dress", "ditch", "gypsy", "pasta", "taffy", "flame", "swoop", "aloof", "sight",
	"broke", "teary", "chart", "sixty", "wordy", "sheer", "leper", "nosey", "bulge", "savor", "clamp", "funky",
	"foamy", "toxic", "brand", "plumb", "dingy", "butte", "drill", "tripe", "bicep", "tenor", "krill", "worse",
	"drama", "hyena", "think", "ratio", "cobra", "basil", "scrum", "bused", "phone", "court", "camel", "proof",
	"heard", "angel", "petal", "pouty", "throb", "maybe", "fetal", "sprig", "spine", "shout", "cadet", "macro",
	"dodgy", "satyr", "rarer", "binge", "trend", "nutty", "leapt", "amiss", "split", "myrrh", "width", "sonar",
	"tower", "baron", "fever", "waver", "spark", "belie", "sloop", "expel", "smote", "baler", "above", "north",
	"wafer", "scant", "frill", "awash", "snack", "scowl", "frail", "drift", "limbo", "fence", "motel", "ounce",
	"wreak", "revel", "talon", "prior", "knelt", "cello", "flake", "debug", "anode", "crime", "salve", "scout",
	"imbue", "pinky", "stave", "vague", "chock", "fight", "video", "stone", "teach", "cleft", "frost", "prawn",
	"booty", "twist", "apnea", "stiff", "plaza", "ledge", "tweak", "board", "grant", "medic", "bacon", "cable",
	"brawl", "slunk", "raspy", "forum", "drone", "women", "mucus", "boast", "toddy", "coven", "tumor", "truer",
	"wrath", "stall", "steam", "axial", "purer", "daily", "trail", "niche", "mealy", "juice", "nylon", "plump",
	"merry", "flail", "papal", "wheat", "berry", "cower", "erect", "brute", "leggy", "snipe", "sinew", "skier",
	"penny", "jumpy", "rally", "umbra", "scary", "modem", "gross", "avian", "greed", "satin", "tonic", "parka",
	"sniff", "livid", "stark", "trump", "giddy", "reuse", "taboo", "avoid", "quote", "devil", "liken", "gloss",
	"gayer", "beret", "noise", "gland", "dealt", "sling", "rumor", "opera", "thigh", "tonga", "flare", "wound",
	"white", "bulky", "etude", "horse", "circa", "paddy", "inbox", "fizzy", "grain", "exert", "surge", "gleam",
	"belle", "salvo", "crush", "fruit", "sappy", "taker", "tract", "ovine", "spiky", "frank", "reedy", "filth",
	"spasm", "heave", "mambo", "right", "clank", "trust", "lumen", "borne", "spook", "sauce", "amber", "lathe",
	"carat", "corer", "dirty", "slyly", "affix", "alloy", "taint", "sheep", "kinky", "wooly", "mauve", "flung",
	"yacht", "fried", "quail", "brunt", "grimy", "curvy", "cagey", "rinse", "deuce", "state", "grasp", "milky",
	"bison", "graft", "sandy", "baste", "flask", "hedge", "girly", "swash", "boney", "coupe", "endow", "abhor",
	"welch", "blade", "tight", "geese", "miser", "mirth", "cloud", "cabal", "leech", "close", "tenth", "pecan",
	"droit", "grail", "clone", "guise", "ralph", "tango", "biddy", "smith", "mower", "payee", "serif", "drape",
	"fifth", "spank", "glaze", "allot", "truck", "kayak", "virus", "testy", "tepee", "fully", "zonal", "metro",
	"curry", "grand", "banjo", "axion", "bezel", "occur", "chain", "nasal", "gooey", "filer", "brace", "allay",
	"pubic", "raven", "plead", "gnash", "flaky", "munch", "dully", "eking", "thing", "slink", "hurry", "theft",
	"shorn", "pygmy", "ranch", "wring", "lemon", "shore", "mamma", "froze", "newer", "style", "moose", "antic",
	"drown", "vegan", "chess", "guppy", "union", "lever", "lorry", "image", "cabby", "druid", "exact", "truth",
	"dopey", "spear", "cried", "chime", "crony", "stunk", "timid", "batch", "gauge", "rotor", "crack", "curve",
	"latte", "witch", "bunch", "repel", "anvil", "soapy", "meter", "broth", "madly", "dried", "scene", "known",
	"magma", "roost", "woman", "thong", "punch", "pasty", "downy", "knead", "whirl", "rapid", "clang", "anger",
	"drive", "goofy", "email", "music", "stuff", "bleep", "rider", "mecca", "folio", "setup", "verso", "quash",
	"fauna", "gummy", "happy", "newly", "fussy", "relic", "guava", "ratty", "fudge", "femur", "chirp", "forte",
	"alibi", "whine", "petty", "golly", "plait", "fleck", "felon", "gourd", "brown", "thrum", "ficus", "stash",
	"decry", "wiser", "junta", "visor", "daunt", "scree", "impel", "await", "press", "whose", "turbo", "stoop",
	"speak", "mangy", "eying", "inlet", "crone", "pulse", "mossy", "staid", "hence", "pinch", "teddy", "sully",
	"snore", "ripen", "snowy", "attic", "going", "leach", "mouth", "hound", "clump", "tonal", "bigot", "peril",
	"piece", "blame", "haute", "spied", "undid", "intro", "basal", "shine", "gecko", "rodeo", "guard", "steer",
	"loamy", "scamp", "scram", "manly", "hello", "vaunt", "organ", "feral", "knock", "extra", "condo", "adapt",
	"willy", "polka", "rayon", "skirt", "faith", "torso", "match", "mercy", "tepid", "sleek", "riser", "twixt",
	"peace", "flush", "catty", "login", "eject", "roger", "rival", "untie", "refit", "aorta", "adult", "judge",
	"rower", "artsy", "rural", "shave", "bobby", "eclat", "fella", "gaily", "harry", "hasty", "hydro", "liege",
	"octal", "ombre", "payer", "sooth", "unset", "unlit", "vomit", "fanny"];

document.getElementById("solve").addEventListener("click", main, false);
document.getElementById("clear").addEventListener("click", clear, false);
document.addEventListener('keydown', logKey);

document.getElementById("word").addEventListener("input", async function () {
	for (var i = 0; i < this.value.length; i++) {
		if (!((this.value.charCodeAt(0) > 64 && this.value.charCodeAt(0) < 91) || (this.value.charCodeAt(0) > 96 && this.value.charCodeAt(0) < 123))) {
			this.value = this.value.substring(0, i) + this.value.substring(i + 1);
		}
	}
	if (this.value.length != 5) {
		document.getElementById("solve").disabled = false;
	}
	else {
		var file = "allWords.txt";
		var rawFile = new XMLHttpRequest();
		rawFile.open("GET", file, false);
		rawFile.onreadystatechange = function () {
			if (rawFile.readyState === 4) {
				if (rawFile.status === 200 || rawFile.status == 0) {
					var allText = rawFile.responseText;
					allText.split("\", \"").forEach(function (element) {
						allWords.push(element);
					});
				}
			}
		}
		rawFile.send(null);
		if (allWords.includes(this.value.toLowerCase())) {
			document.getElementById("messages1").innerHTML = "Enter each of your guesses <br> Total Possible Guesses: 12967";
			document.getElementById("100").focus();

			initStructures();
			var arr2 = getEligible(false);
			theAnswer = String(this.value).toLowerCase();
			var index = 0;
			var wordGuesses = [];
			if (results != 1) {
				arr2.forEach(async function (value, key) {
					index++;
					if (index <= 6) {
						wordGuesses.push(value);
					}
				});
				for (var j = 0; j < wordGuesses.length; j++) {
					inLoop = true;
					var wordGuess2 = wordGuesses[j];
					for (var i = 0; i < wordGuess2.length; i++) {
						var id = "2" + (j) + "" + i;
						colorResult = getColors(wordGuess2);
						document.getElementById(id).value = ("" + wordGuess2.charAt(i)).toUpperCase();
						if (colorResult.charAt(i) == 'G')
							document.getElementById(id).style.backgroundColor = "#6aaa64";
						if (colorResult.charAt(i) == 'Y')
							document.getElementById(id).style.backgroundColor = "#c9b458";
						if (colorResult.charAt(i) == 'B')
							document.getElementById(id).style.backgroundColor = "#787c7e";
						document.getElementById(id).style.color = "white";
						await new Promise(r => setTimeout(r, 10));
					}
					await new Promise(r => setTimeout(r, 20));

				}
				inLoop = false;
			}
			this.style.color = "white";
			this.style.backgroundColor = "#787c7e";
		}
		else {
			document.getElementById("messages1").innerHTML = "The word you entered is not in the dictionary. Please try again.";
			this.value = "";
		}
		allWords = [];
		this.value = this.value.toUpperCase();

	}
}, true);

document.getElementById("100").addEventListener("input", function () {

	var currentIndex = "100";
	if (!inLoop && ((this.value.charCodeAt(0) > 64 && this.value.charCodeAt(0) < 91) || (this.value.charCodeAt(0) > 96 && this.value.charCodeAt(0) < 123))) {
		if (document.getElementById(currentIndex).value.length == 1) {

			index = parseInt(currentIndex)
			if (index % 10 < 4) {
				document.getElementById(index + 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length == 0) {
			index = parseInt(currentIndex)
			if (index % 10 > 0) {

				document.getElementById(index - 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length > 1) {
			document.getElementById(currentIndex).value = document.getElementById(currentIndex).value.charAt(0);
		}
	}
	else {
		this.value = "";
	}

}, true);
document.getElementById("101").addEventListener("input", function () {

	var currentIndex = "101";

	if (!inLoop && ((this.value.charCodeAt(0) > 64 && this.value.charCodeAt(0) < 91) || (this.value.charCodeAt(0) > 96 && this.value.charCodeAt(0) < 123))) {

		if (document.getElementById(currentIndex).value.length == 1) {
			index = parseInt(currentIndex)
			if (index % 10 < 4) {

				document.getElementById(index + 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length > 1) {
			document.getElementById(currentIndex).value = document.getElementById(currentIndex).value.charAt(0);
		}
	}
	else {
		this.value = "";
	}

}, true);
document.getElementById("102").addEventListener("input", function () {

	var currentIndex = "102";
	if (!inLoop && ((this.value.charCodeAt(0) > 64 && this.value.charCodeAt(0) < 91) || (this.value.charCodeAt(0) > 96 && this.value.charCodeAt(0) < 123))) {


		if (document.getElementById(currentIndex).value.length == 1) {

			index = parseInt(currentIndex)
			if (index % 10 < 4) {

				document.getElementById(index + 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length == 0) {
			index = parseInt(currentIndex)
			if (index % 10 > 0) {

				document.getElementById(index - 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length > 1) {
			document.getElementById(currentIndex).value = document.getElementById(currentIndex).value.charAt(0);
		}
	}
	else {
		this.value = "";
	}

}, true);
document.getElementById("103").addEventListener("input", function () {
	var currentIndex = "103";
	if (!inLoop && ((this.value.charCodeAt(0) > 64 && this.value.charCodeAt(0) < 91) || (this.value.charCodeAt(0) > 96 && this.value.charCodeAt(0) < 123))) {


		if (document.getElementById(currentIndex).value.length == 1) {

			index = parseInt(currentIndex)
			if (index % 10 < 4) {

				document.getElementById(index + 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length == 0) {
			index = parseInt(currentIndex)
			if (index % 10 > 0) {

				document.getElementById(index - 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length > 1) {
			document.getElementById(currentIndex).value = document.getElementById(currentIndex).value.charAt(0);
		}
	}
	else {
		this.value = "";
	}

}, true);
document.getElementById("104").addEventListener("input", function () {
	var currentIndex = "104";
	if (!inLoop && ((this.value.charCodeAt(0) > 64 && this.value.charCodeAt(0) < 91) || (this.value.charCodeAt(0) > 96 && this.value.charCodeAt(0) < 123))) {

		var isComplete = true;
		for (var i = 0; i < 5; i++) {
			if (document.getElementById("10" + i).value.length != 1) {
				isComplete = false;

			}
		}

		if (isComplete) {
			initStructures();
			var theFinalWord = String(document.getElementById("100").value) + String(document.getElementById("101").value) + String(document.getElementById("102").value) + String(document.getElementById("103").value) + String(document.getElementById("104").value);
			if (allWords.includes(theFinalWord.toLowerCase())) {

				computePlayer(String(document.getElementById("100").value) + String(document.getElementById("101").value) + String(document.getElementById("102").value) + String(document.getElementById("103").value) + String(document.getElementById("104").value))
				inLoop = true;
				if (results != 1 && String(document.getElementById("word").value).toLowerCase().length == 5) {
					document.getElementById("" + (parseInt(currentIndex) + 6)).focus();
					for (var i = 0; i < 6; i++) {
						for (var j = 0; j < 5; j++) {
							if (i != 1) {
								document.getElementById("1" + i + j).setAttribute("readonly", true);
								document.getElementById("1" + i + j).removeAttribute("focusable");
								document.getElementById("1" + i + j).setAttribute("tabindex", -1);
							}
							else {
								document.getElementById("1" + i + j).removeAttribute("readonly");
								document.getElementById("1" + i + j).setAttribute("tabindex", 1);
								document.getElementById("1" + i + j).setAttribute("focusable", true);
							}
						}
					}
				}
				else if (String(document.getElementById("word").value).toLowerCase().length != 5) {
					document.getElementById("word").focus();
				}
			}
			else {
				if (!document.getElementById("messages1").innerHTML.includes("dictionary")) {
					document.getElementById("messages1").innerHTML += "<br>The word you entered is not in the dictionary. Please try again.";
				}
				for (var i = 0; i < 5; i++) {
					document.getElementById("1" + "0" + i).value = "";
				}
				document.getElementById("100").focus();
			}
		}
		if (document.getElementById(currentIndex).value.length > 1) {
			document.getElementById(currentIndex).value = document.getElementById(currentIndex).value.charAt(0);
		}
	}
	else {
		this.value = "";
	}

	//WORD COMPLETE
	//COMPUTE
	//ACTIVATE OTHER BOXES
}, true);

document.getElementById("110").addEventListener("input", function () {
	var currentIndex = "110";

	if (!inLoop && ((this.value.charCodeAt(0) > 64 && this.value.charCodeAt(0) < 91) || (this.value.charCodeAt(0) > 96 && this.value.charCodeAt(0) < 123))) {

		if (document.getElementById(currentIndex).value.length == 1) {

			index = parseInt(currentIndex)
			if (index % 10 < 4) {

				document.getElementById(index + 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length == 0) {
			index = parseInt(currentIndex)
			if (index % 10 > 0) {

				document.getElementById(index - 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length > 1) {
			document.getElementById(currentIndex).value = document.getElementById(currentIndex).value.charAt(0);
		}
	}
	else {
		this.value = "";
	}
}, true);
document.getElementById("111").addEventListener("input", function () {
	var currentIndex = "111";

	if (!inLoop && ((this.value.charCodeAt(0) > 64 && this.value.charCodeAt(0) < 91) || (this.value.charCodeAt(0) > 96 && this.value.charCodeAt(0) < 123))) {

		if (document.getElementById(currentIndex).value.length == 1) {

			index = parseInt(currentIndex)
			if (index % 10 < 4) {

				document.getElementById(index + 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length == 0) {
			index = parseInt(currentIndex)
			if (index % 10 > 0) {

				document.getElementById(index - 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length > 1) {
			document.getElementById(currentIndex).value = document.getElementById(currentIndex).value.charAt(0);
		}
	}
	else {
		this.value = "";
	}
}, true);
document.getElementById("112").addEventListener("input", function () {
	var currentIndex = "112";
	if (!inLoop && ((this.value.charCodeAt(0) > 64 && this.value.charCodeAt(0) < 91) || (this.value.charCodeAt(0) > 96 && this.value.charCodeAt(0) < 123))) {


		if (document.getElementById(currentIndex).value.length == 1) {

			index = parseInt(currentIndex)
			if (index % 10 < 4) {

				document.getElementById(index + 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length == 0) {
			index = parseInt(currentIndex)
			if (index % 10 > 0) {

				document.getElementById(index - 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length > 1) {
			document.getElementById(currentIndex).value = document.getElementById(currentIndex).value.charAt(0);
		}
	}
	else {
		this.value = "";
	}
}, true);
document.getElementById("113").addEventListener("input", function () {
	var currentIndex = "113";

	if (!inLoop && ((this.value.charCodeAt(0) > 64 && this.value.charCodeAt(0) < 91) || (this.value.charCodeAt(0) > 96 && this.value.charCodeAt(0) < 123))) {

		if (document.getElementById(currentIndex).value.length == 1) {

			index = parseInt(currentIndex)
			if (index % 10 < 4) {

				document.getElementById(index + 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length == 0) {
			index = parseInt(currentIndex)
			if (index % 10 > 0) {

				document.getElementById(index - 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length > 1) {
			document.getElementById(currentIndex).value = document.getElementById(currentIndex).value.charAt(0);
		}
	}
	else {
		this.value = "";
	}
}, true);
document.getElementById("114").addEventListener("input", async function () {

	var currentIndex = "114";
	if (!inLoop && ((this.value.charCodeAt(0) > 64 && this.value.charCodeAt(0) < 91) || (this.value.charCodeAt(0) > 96 && this.value.charCodeAt(0) < 123))) {
		var isComplete = true;
		for (var i = 0; i < 5; i++) {
			if (document.getElementById("11" + i).value.length != 1) {
				isComplete = false;

			}
		}

		if (isComplete) {
			var theFinalWord = String(document.getElementById("110").value) + String(document.getElementById("111").value) + String(document.getElementById("112").value) + String(document.getElementById("113").value) + String(document.getElementById("114").value);
			if (allWords.includes(theFinalWord.toLowerCase())) {

				computePlayer(String(document.getElementById("110").value) + String(document.getElementById("111").value) + String(document.getElementById("112").value) + String(document.getElementById("113").value) + String(document.getElementById("114").value))
				inLoop = true;
				if (results != 1) {
					document.getElementById("" + (parseInt(currentIndex) + 6)).focus();
					for (var i = 0; i < 6; i++) {
						for (var j = 0; j < 5; j++) {
							if (i != 2) {
								document.getElementById("1" + i + j).setAttribute("readonly", true);
								document.getElementById("1" + i + j).removeAttribute("focusable");
								document.getElementById("1" + i + j).setAttribute("tabindex", -1);
							}
							else {
								document.getElementById("1" + i + j).removeAttribute("readonly");
								document.getElementById("1" + i + j).setAttribute("tabindex", 1);
								document.getElementById("1" + i + j).setAttribute("focusable", true);
							}
						}
					}
				}
			}
			else {
				if (!document.getElementById("messages1").innerHTML.includes("dictionary")) {
					document.getElementById("messages1").innerHTML += "<br>The word you entered is not in the dictionary. Please try again.";
				} for (var i = 0; i < 5; i++) {
					document.getElementById("1" + "1" + i).value = "";
				}
				document.getElementById("110").focus();
			}
		}
		if (document.getElementById(currentIndex).value.length > 1) {
			document.getElementById(currentIndex).value = document.getElementById(currentIndex).value.charAt(0);
		}
	}
	else {
		this.value = "";
	}

	//WORD COMPLETE
	//COMPUTE
	//ACTIVATE OTHER BOXES

}, true);

document.getElementById("120").addEventListener("input", function () {
	var currentIndex = "120";

	if (!inLoop && ((this.value.charCodeAt(0) > 64 && this.value.charCodeAt(0) < 91) || (this.value.charCodeAt(0) > 96 && this.value.charCodeAt(0) < 123))) {

		if (document.getElementById(currentIndex).value.length == 1) {

			index = parseInt(currentIndex)
			if (index % 10 < 4) {

				document.getElementById(index + 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length == 0) {
			index = parseInt(currentIndex)
			if (index % 10 > 0) {

				document.getElementById(index - 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length > 1) {
			document.getElementById(currentIndex).value = document.getElementById(currentIndex).value.charAt(0);
		}
	}
	else {
		this.value = "";
	}
}, true);
document.getElementById("121").addEventListener("input", function () {
	var currentIndex = "121";

	if (!inLoop && ((this.value.charCodeAt(0) > 64 && this.value.charCodeAt(0) < 91) || (this.value.charCodeAt(0) > 96 && this.value.charCodeAt(0) < 123))) {

		if (document.getElementById(currentIndex).value.length == 1) {

			index = parseInt(currentIndex)
			if (index % 10 < 4) {

				document.getElementById(index + 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length == 0) {
			index = parseInt(currentIndex)
			if (index % 10 > 0) {

				document.getElementById(index - 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length > 1) {
			document.getElementById(currentIndex).value = document.getElementById(currentIndex).value.charAt(0);
		}
	}
	else {
		this.value = "";
	}
}, true);
document.getElementById("122").addEventListener("input", function () {
	var currentIndex = "122";

	if (!inLoop && ((this.value.charCodeAt(0) > 64 && this.value.charCodeAt(0) < 91) || (this.value.charCodeAt(0) > 96 && this.value.charCodeAt(0) < 123))) {

		if (document.getElementById(currentIndex).value.length == 1) {

			index = parseInt(currentIndex)
			if (index % 10 < 4) {

				document.getElementById(index + 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length == 0) {
			index = parseInt(currentIndex)
			if (index % 10 > 0) {

				document.getElementById(index - 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length > 1) {
			document.getElementById(currentIndex).value = document.getElementById(currentIndex).value.charAt(0);
		}
	}
	else {
		this.value = "";
	}
}, true);
document.getElementById("123").addEventListener("input", function () {
	var currentIndex = "123";

	if (!inLoop && ((this.value.charCodeAt(0) > 64 && this.value.charCodeAt(0) < 91) || (this.value.charCodeAt(0) > 96 && this.value.charCodeAt(0) < 123))) {

		if (document.getElementById(currentIndex).value.length == 1) {

			index = parseInt(currentIndex)
			if (index % 10 < 4) {

				document.getElementById(index + 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length == 0) {
			index = parseInt(currentIndex)
			if (index % 10 > 0) {

				document.getElementById(index - 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length > 1) {
			document.getElementById(currentIndex).value = document.getElementById(currentIndex).value.charAt(0);
		}
	}
	else {
		this.value = "";
	}
}, true);
document.getElementById("124").addEventListener("input", function () {
	var currentIndex = "124";
	if (!inLoop && ((this.value.charCodeAt(0) > 64 && this.value.charCodeAt(0) < 91) || (this.value.charCodeAt(0) > 96 && this.value.charCodeAt(0) < 123))) {
		var isComplete = true;
		for (var i = 0; i < 5; i++) {
			if (document.getElementById("12" + i).value.length != 1) {
				isComplete = false;

			}
		}

		if (isComplete) {
			var theFinalWord = String(document.getElementById("120").value) + String(document.getElementById("121").value) + String(document.getElementById("122").value) + String(document.getElementById("123").value) + String(document.getElementById("124").value);
			if (allWords.includes(theFinalWord.toLowerCase())) {


				computePlayer(String(document.getElementById("120").value) + String(document.getElementById("121").value) + String(document.getElementById("122").value) + String(document.getElementById("123").value) + String(document.getElementById("124").value))
				inLoop = true;
				if (results != 1) {
					document.getElementById("" + (parseInt(currentIndex) + 6)).focus();
					for (var i = 0; i < 6; i++) {
						for (var j = 0; j < 5; j++) {
							if (i != 3) {
								document.getElementById("1" + i + j).setAttribute("readonly", true);
								document.getElementById("1" + i + j).removeAttribute("focusable");
								document.getElementById("1" + i + j).setAttribute("tabindex", -1);
							}
							else {
								document.getElementById("1" + i + j).removeAttribute("readonly");
								document.getElementById("1" + i + j).setAttribute("tabindex", 1);
								document.getElementById("1" + i + j).setAttribute("focusable", true);
							}
						}
					}
				}
			}
			else {
				if (!document.getElementById("messages1").innerHTML.includes("dictionary")) {
					document.getElementById("messages1").innerHTML += "<br>The word you entered is not in the dictionary. Please try again.";
				} for (var i = 0; i < 5; i++) {
					document.getElementById("1" + "2" + i).value = "";
				}
				document.getElementById("120").focus();
			}
		}
		if (document.getElementById(currentIndex).value.length > 1) {
			document.getElementById(currentIndex).value = document.getElementById(currentIndex).value.charAt(0);
		}
	}
	else {
		this.value = "";
	}

	//WORD COMPLETE
	//COMPUTE
	//ACTIVATE OTHER BOXES

}, true);

document.getElementById("130").addEventListener("input", function () {
	var currentIndex = "130";

	if (!inLoop && ((this.value.charCodeAt(0) > 64 && this.value.charCodeAt(0) < 91) || (this.value.charCodeAt(0) > 96 && this.value.charCodeAt(0) < 123))) {

		if (document.getElementById(currentIndex).value.length == 1) {

			index = parseInt(currentIndex)
			if (index % 10 < 4) {

				document.getElementById(index + 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length == 0) {
			index = parseInt(currentIndex)
			if (index % 10 > 0) {

				document.getElementById(index - 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length > 1) {
			document.getElementById(currentIndex).value = document.getElementById(currentIndex).value.charAt(0);
		}
	}
	else {
		this.value = "";
	}
}, true);
document.getElementById("131").addEventListener("input", function () {
	var currentIndex = "131";

	if (!inLoop && ((this.value.charCodeAt(0) > 64 && this.value.charCodeAt(0) < 91) || (this.value.charCodeAt(0) > 96 && this.value.charCodeAt(0) < 123))) {

		if (document.getElementById(currentIndex).value.length == 1) {

			index = parseInt(currentIndex)
			if (index % 10 < 4) {

				document.getElementById(index + 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length == 0) {
			index = parseInt(currentIndex)
			if (index % 10 > 0) {

				document.getElementById(index - 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length > 1) {
			document.getElementById(currentIndex).value = document.getElementById(currentIndex).value.charAt(0);
		}
	}
	else {
		this.value = "";
	}
}, true);
document.getElementById("132").addEventListener("input", function () {
	var currentIndex = "132";
	if (!inLoop && ((this.value.charCodeAt(0) > 64 && this.value.charCodeAt(0) < 91) || (this.value.charCodeAt(0) > 96 && this.value.charCodeAt(0) < 123))) {


		if (document.getElementById(currentIndex).value.length == 1) {

			index = parseInt(currentIndex)
			if (index % 10 < 4) {

				document.getElementById(index + 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length == 0) {
			index = parseInt(currentIndex)
			if (index % 10 > 0) {

				document.getElementById(index - 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length > 1) {
			document.getElementById(currentIndex).value = document.getElementById(currentIndex).value.charAt(0);
		}
	}
	else {
		this.value = "";
	}
}, true);
document.getElementById("133").addEventListener("input", function () {
	var currentIndex = "133";
	if (!inLoop && ((this.value.charCodeAt(0) > 64 && this.value.charCodeAt(0) < 91) || (this.value.charCodeAt(0) > 96 && this.value.charCodeAt(0) < 123))) {


		if (document.getElementById(currentIndex).value.length == 1) {

			index = parseInt(currentIndex)
			if (index % 10 < 4) {

				document.getElementById(index + 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length == 0) {
			index = parseInt(currentIndex)
			if (index % 10 > 0) {

				document.getElementById(index - 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length > 1) {
			document.getElementById(currentIndex).value = document.getElementById(currentIndex).value.charAt(0);
		}
	}
	else {
		this.value = "";
	}
}, true);
document.getElementById("134").addEventListener("input", function () {
	var currentIndex = "134";
	if (!inLoop && ((this.value.charCodeAt(0) > 64 && this.value.charCodeAt(0) < 91) || (this.value.charCodeAt(0) > 96 && this.value.charCodeAt(0) < 123))) {
		var isComplete = true;
		for (var i = 0; i < 5; i++) {
			if (document.getElementById("13" + i).value.length != 1) {
				isComplete = false;

			}
		}

		if (isComplete) {
			var theFinalWord = String(document.getElementById("130").value) + String(document.getElementById("131").value) + String(document.getElementById("132").value) + String(document.getElementById("133").value) + String(document.getElementById("134").value);
			if (allWords.includes(theFinalWord.toLowerCase())) {


				computePlayer(String(document.getElementById("130").value) + String(document.getElementById("131").value) + String(document.getElementById("132").value) + String(document.getElementById("133").value) + String(document.getElementById("134").value))
				inLoop = true;
				if (results != 1) {
					document.getElementById("" + (parseInt(currentIndex) + 6)).focus();
					for (var i = 0; i < 6; i++) {
						for (var j = 0; j < 5; j++) {
							if (i != 4) {
								document.getElementById("1" + i + j).setAttribute("readonly", true);
								document.getElementById("1" + i + j).removeAttribute("focusable");
								document.getElementById("1" + i + j).setAttribute("tabindex", -1);
							}
							else {
								document.getElementById("1" + i + j).removeAttribute("readonly");
								document.getElementById("1" + i + j).setAttribute("tabindex", 1);
								document.getElementById("1" + i + j).setAttribute("focusable", true);
							}
						}
					}
				}
			}
			else {
				if (!document.getElementById("messages1").innerHTML.includes("dictionary")) {
					document.getElementById("messages1").innerHTML += "<br>The word you entered is not in the dictionary. Please try again.";
				} for (var i = 0; i < 5; i++) {
					document.getElementById("1" + "3" + i).value = "";
				}
				document.getElementById("130").focus();
			}
		}

		if (document.getElementById(currentIndex).value.length > 1) {
			document.getElementById(currentIndex).value = document.getElementById(currentIndex).value.charAt(0);
		}
	}
	else {
		this.value = "";
	}

	//WORD COMPLETE
	//COMPUTE
	//ACTIVATE OTHER BOXES

}, true);

document.getElementById("140").addEventListener("input", function () {
	var currentIndex = "140";
	if (!inLoop && ((this.value.charCodeAt(0) > 64 && this.value.charCodeAt(0) < 91) || (this.value.charCodeAt(0) > 96 && this.value.charCodeAt(0) < 123))) {

		if (document.getElementById(currentIndex).value.length == 1) {

			index = parseInt(currentIndex)
			if (index % 10 < 4) {

				document.getElementById(index + 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length == 0) {
			index = parseInt(currentIndex)
			if (index % 10 > 0) {

				document.getElementById(index - 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length > 1) {
			document.getElementById(currentIndex).value = document.getElementById(currentIndex).value.charAt(0);
		}
	}
	else {
		this.value = "";
	}
}, true);
document.getElementById("141").addEventListener("input", function () {
	var currentIndex = "141";

	if (!inLoop && ((this.value.charCodeAt(0) > 64 && this.value.charCodeAt(0) < 91) || (this.value.charCodeAt(0) > 96 && this.value.charCodeAt(0) < 123))) {

		if (document.getElementById(currentIndex).value.length == 1) {

			index = parseInt(currentIndex)
			if (index % 10 < 4) {

				document.getElementById(index + 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length == 0) {
			index = parseInt(currentIndex)
			if (index % 10 > 0) {

				document.getElementById(index - 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length > 1) {
			document.getElementById(currentIndex).value = document.getElementById(currentIndex).value.charAt(0);
		}
	}
	else {
		this.value = "";
	}
}, true);
document.getElementById("142").addEventListener("input", function () {
	var currentIndex = "142";

	if (!inLoop && ((this.value.charCodeAt(0) > 64 && this.value.charCodeAt(0) < 91) || (this.value.charCodeAt(0) > 96 && this.value.charCodeAt(0) < 123))) {

		if (document.getElementById(currentIndex).value.length == 1) {

			index = parseInt(currentIndex)
			if (index % 10 < 4) {

				document.getElementById(index + 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length == 0) {
			index = parseInt(currentIndex)
			if (index % 10 > 0) {

				document.getElementById(index - 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length > 1) {
			document.getElementById(currentIndex).value = document.getElementById(currentIndex).value.charAt(0);
		}
	}
	else {
		this.value = "";
	}
}, true);
document.getElementById("143").addEventListener("input", function () {
	var currentIndex = "143";

	if (!inLoop && ((this.value.charCodeAt(0) > 64 && this.value.charCodeAt(0) < 91) || (this.value.charCodeAt(0) > 96 && this.value.charCodeAt(0) < 123))) {

		if (document.getElementById(currentIndex).value.length == 1) {

			index = parseInt(currentIndex)
			if (index % 10 < 4) {

				document.getElementById(index + 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length == 0) {
			index = parseInt(currentIndex)
			if (index % 10 > 0) {

				document.getElementById(index - 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length > 1) {
			document.getElementById(currentIndex).value = document.getElementById(currentIndex).value.charAt(0);
		}
	}
	else {
		this.value = "";
	}
}, true);
document.getElementById("144").addEventListener("input", function () {

	var currentIndex = "144";
	if (!inLoop && ((this.value.charCodeAt(0) > 64 && this.value.charCodeAt(0) < 91) || (this.value.charCodeAt(0) > 96 && this.value.charCodeAt(0) < 123))) {
		var isComplete = true;
		for (var i = 0; i < 5; i++) {
			if (document.getElementById("14" + i).value.length != 1) {
				isComplete = false;
			}
		}

		if (isComplete) {
			var theFinalWord = String(document.getElementById("140").value) + String(document.getElementById("141").value) + String(document.getElementById("142").value) + String(document.getElementById("143").value) + String(document.getElementById("144").value);
			if (allWords.includes(theFinalWord.toLowerCase())) {

				computePlayer(String(document.getElementById("140").value) + String(document.getElementById("141").value) + String(document.getElementById("142").value) + String(document.getElementById("143").value) + String(document.getElementById("144").value))
				inLoop = true;
				if (results != 1) {
					document.getElementById("" + (parseInt(currentIndex) + 6)).focus();
					for (var i = 0; i < 6; i++) {
						for (var j = 0; j < 5; j++) {
							if (i != 5) {
								document.getElementById("1" + i + j).setAttribute("readonly", true);
								document.getElementById("1" + i + j).removeAttribute("focusable");
								document.getElementById("1" + i + j).setAttribute("tabindex", -1);
							}
							else {
								document.getElementById("1" + i + j).removeAttribute("readonly");
								document.getElementById("1" + i + j).setAttribute("tabindex", 1);
								document.getElementById("1" + i + j).setAttribute("focusable", true);
							}
						}
					}
				}
			}
			else {
				if (!document.getElementById("messages1").innerHTML.includes("dictionary")) {
					document.getElementById("messages1").innerHTML += "<br>The word you entered is not in the dictionary. Please try again.";
				} for (var i = 0; i < 5; i++) {
					document.getElementById("1" + "4" + i).value = "";
				}
				document.getElementById("140").focus();
			}
		}
		if (document.getElementById(currentIndex).value.length > 1) {
			document.getElementById(currentIndex).value = document.getElementById(currentIndex).value.charAt(0);
		}
	}
	else {
		this.value = "";
	}
}

	//WORD COMPLETE
	//COMPUTE
	//ACTIVATE OTHER BOXES
	, true);

document.getElementById("150").addEventListener("input", function () {
	var currentIndex = "150";

	if (!inLoop && ((this.value.charCodeAt(0) > 64 && this.value.charCodeAt(0) < 91) || (this.value.charCodeAt(0) > 96 && this.value.charCodeAt(0) < 123))) {

		if (document.getElementById(currentIndex).value.length == 1) {

			index = parseInt(currentIndex)
			if (index % 10 < 4) {

				document.getElementById(index + 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length == 0) {
			index = parseInt(currentIndex)
			if (index % 10 > 0) {

				document.getElementById(index - 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length > 1) {
			document.getElementById(currentIndex).value = document.getElementById(currentIndex).value.charAt(0);
		}
	}
	else {
		this.value = "";
	}
}, true);
document.getElementById("151").addEventListener("input", function () {
	var currentIndex = "151";

	if (!inLoop && ((this.value.charCodeAt(0) > 64 && this.value.charCodeAt(0) < 91) || (this.value.charCodeAt(0) > 96 && this.value.charCodeAt(0) < 123))) {

		if (document.getElementById(currentIndex).value.length == 1) {

			index = parseInt(currentIndex)
			if (index % 10 < 4) {

				document.getElementById(index + 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length == 0) {
			index = parseInt(currentIndex)
			if (index % 10 > 0) {

				document.getElementById(index - 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length > 1) {
			document.getElementById(currentIndex).value = document.getElementById(currentIndex).value.charAt(0);
		}
	}
	else {
		this.value = "";
	}
}, true);
document.getElementById("152").addEventListener("input", function () {
	var currentIndex = "152";

	if (!inLoop && ((this.value.charCodeAt(0) > 64 && this.value.charCodeAt(0) < 91) || (this.value.charCodeAt(0) > 96 && this.value.charCodeAt(0) < 123))) {

		if (document.getElementById(currentIndex).value.length == 1) {

			index = parseInt(currentIndex)
			if (index % 10 < 4) {

				document.getElementById(index + 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length == 0) {
			index = parseInt(currentIndex)
			if (index % 10 > 0) {

				document.getElementById(index - 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length > 1) {
			document.getElementById(currentIndex).value = document.getElementById(currentIndex).value.charAt(0);
		}
	}
	else {
		this.value = "";
	}
}, true);
document.getElementById("153").addEventListener("input", function () {
	var currentIndex = "153";

	if (!inLoop && ((this.value.charCodeAt(0) > 64 && this.value.charCodeAt(0) < 91) || (this.value.charCodeAt(0) > 96 && this.value.charCodeAt(0) < 123))) {

		if (document.getElementById(currentIndex).value.length == 1) {

			index = parseInt(currentIndex)
			if (index % 10 < 4) {

				document.getElementById(index + 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length == 0) {
			index = parseInt(currentIndex)
			if (index % 10 > 0) {

				document.getElementById(index - 1).focus();
			}
		}
		else if (document.getElementById(currentIndex).value.length > 1) {
			document.getElementById(currentIndex).value = document.getElementById(currentIndex).value.charAt(0);
		}
	}
	else {
		this.value = "";
	}
}, true);
document.getElementById("154").addEventListener("input", function () {
	var currentIndex = "154";
	if (!inLoop && ((this.value.charCodeAt(0) > 64 && this.value.charCodeAt(0) < 91) || (this.value.charCodeAt(0) > 96 && this.value.charCodeAt(0) < 123))) {
		var isComplete = true;
		for (var i = 0; i < 5; i++) {
			if (document.getElementById("15" + i).value.length != 1) {
				isComplete = false;

			}
		}
		if (isComplete) {
			var theFinalWord = String(document.getElementById("150").value) + String(document.getElementById("151").value) + String(document.getElementById("152").value) + String(document.getElementById("153").value) + String(document.getElementById("154").value);
			if (allWords.includes(theFinalWord.toLowerCase())) {


				computePlayer(String(document.getElementById("150").value) + String(document.getElementById("151").value) + String(document.getElementById("152").value) + String(document.getElementById("153").value) + String(document.getElementById("154").value))
				inLoop = true;
				if (results != 1) {
					for (var i = 0; i < 6; i++) {
						for (var j = 0; j < 5; j++) {
							if (i != 6) {
								document.getElementById("1" + i + j).setAttribute("readonly", true);
								document.getElementById("1" + i + j).removeAttribute("focusable");
								document.getElementById("1" + i + j).setAttribute("tabindex", -1);
							}
							else {
								document.getElementById("1" + i + j).removeAttribute("readonly");
								document.getElementById("1" + i + j).setAttribute("tabindex", 1);
								document.getElementById("1" + i + j).setAttribute("focusable", true);
							}
						}
					}
					document.getElementById("yours").innerHTML = "Your Solution - " + wordCounter + "/6";
					document.getElementById("spacer1").style.width = "80px";
					document.getElementById("spacer2").style.width = "70px";
					giveFeedback();
					document.getElementById("solve").click();
					document.getElementById("clear").focus();
				}
			}
			else {
				if (!document.getElementById("messages1").innerHTML.includes("dictionary")) {
					document.getElementById("messages1").innerHTML += "<br>The word you entered is not in the dictionary. Please try again.";
				} for (var i = 0; i < 5; i++) {
					document.getElementById("1" + "5" + i).value = "";
				}
				document.getElementById("150").focus();
			}
		}
		if (document.getElementById(currentIndex).value.length > 1) {
			document.getElementById(currentIndex).value = document.getElementById(currentIndex).value.charAt(0);
		}
	}
	else {
		this.value = "";
	}

	//WORD COMPLETE
	//COMPUTE
	//ACTIVATE OTHER BOXES

}, true);

function logKey(e) {
	if (e.keyCode == 8) {
		if (document.activeElement.id.length == 3) {
			//	if (document.activeElement.id.charAt(2) != "3") {
			if (document.activeElement.value == "") {
				document.getElementById(parseInt(document.activeElement.id) - 1).value = "";
				document.getElementById(parseInt(document.activeElement.id) - 1).focus();
			}
		}
		//}
	}
}

function computePlayer(word) {
	document.getElementById("word").setAttribute("readonly", true);
	theAnswer = String(document.getElementById("word").value).toLowerCase();
	if (theAnswer.length == 5) {
		recur(word.toLowerCase(), false);
	}
	else {
		clear();
		document.getElementById("messages1").innerHTML = "Please enter a 5 letter word";
	}
}

function clear() {
	results = 0;
	document.getElementById("solve").disabled = false;
	for (var i = 0; i < 6; i++) {
		for (var j = 0; j < 5; j++) {
			document.getElementById("3" + i + j).style.backgroundColor = "white";
			document.getElementById("1" + i + j).style.backgroundColor = "white";
			document.getElementById("2" + i + j).style.backgroundColor = "white";
			document.getElementById("1" + i + j).style.color = "black";
			document.getElementById("3" + i + j).value = "";
			document.getElementById("1" + i + j).value = "";
			document.getElementById("2" + i + j).value = "";
		}
	}
	document.getElementById("messages1").innerHTML = "";
	for (var i = 0; i < 6; i++) {
		for (var j = 0; j < 5; j++) {
			if (i != 0) {
				document.getElementById("1" + i + j).setAttribute("readonly", true);
				document.getElementById("1" + i + j).removeAttribute("focusable");
				document.getElementById("1" + i + j).setAttribute("tabindex", -1);
			}
			else {
				document.getElementById("1" + i + j).removeAttribute("readonly");
				document.getElementById("1" + i + j).setAttribute("tabindex", 1);
				document.getElementById("1" + i + j).setAttribute("focusable", true);
			}
		}
	}
	document.getElementById("messages1").innerHTML = "Enter today's word first";
	document.getElementById("messages3").innerHTML = "FEEDBACK";
	document.getElementById("yours").innerHTML = "Your Wordle Solution";
	document.getElementById("comp").innerHTML = "Optimal Solution";
	document.getElementById("spacer0").style.width = "95px";
	document.getElementById("spacer1").style.width = "60px";
	document.getElementById("spacer2").style.width = "70px";
	document.getElementById("word").value = "";
	document.getElementById("word").focus();
	document.getElementById("word").style.backgroundColor = "white";
	document.getElementById("word").style.color = "black";
	document.getElementById("word").removeAttribute("readonly");
}

function main() {
	document.getElementById("solve").disabled = true;
	initStructures();
	word = String(document.getElementById("word").value);
	if (word.length == 5) {
		document.getElementById("word").setAttribute("readonly", true);
		recur(word, true);
		if (results != 1) {
			var theFirstIndex = 0;
			while (theFirstIndex < 6 && document.getElementById("1" + String(theFirstIndex) + "3").getAttribute("readonly") == "true") {
				theFirstIndex++;
			}
			if (theFirstIndex < 6) {
				document.getElementById("1" + String(theFirstIndex) + "3").focus();
			}
		}
		else
			document.getElementById("clear").focus();
	}
	else {
		clear();
		document.getElementById("solve").disabled = false;
		document.getElementById("messages1").innerHTML = "Please enter a 5 letter word";
		document.getElementById("word").focus();
	}
}

function initStructures() {
	allWords = [];
	greenLetters = new Map();
	yellowLetters = new Map();
	blackLetters = new Map();
	eligibleWords = new Map();
	allWordFreq = new Map();
	words = [];
	letters = [];
	chars = [];
	wordGuess = "";
	inLoop = false;
	colorResult = "";
	prevArrSize = 12967;
	reportleMultiplier = 1;
	wordCounter = 0;
	theAnswer = "";
	vals = [];
	reportle = 0;
	allLetterFrequencyUser = 0;
	allPositionUser = 0;
	allWordFrequencyUser = 0;
	allLetterFrequencyComp = 0;
	allPositionComp = 0;
	allWordFrequencyComp = 0;
	for (var i = 0; i < 6; i++) {
		for (var j = 0; j < 5; j++) {
			document.getElementById("3" + i + j).style.backgroundColor = "000000";
			document.getElementById("1" + i + j).style.backgroundColor = "000000";
			document.getElementById("2" + i + j).style.backgroundColor = "000000";
		}
	}

	var file = "allWords.txt";

	var rawFile = new XMLHttpRequest();
	rawFile.open("GET", file, false);
	rawFile.onreadystatechange = function () {
		if (rawFile.readyState === 4) {
			if (rawFile.status === 200 || rawFile.status == 0) {
				var allText = rawFile.responseText;
				allText.split("\", \"").forEach(function (element) {
					allWords.push(element);
				});
			}
		}
	}
	rawFile.send(null);

	for (var i = 0; i < 52; i++) {
		letters.push([0, 0, 0, 0, 0]);
		chars.push(0);
	}

	storAllWords.forEach(function (element) {
		if (element != null) {
			for (var k = 0; k < element.length; k++) {
				var j = "" + element.charAt(k);
				if (element.indexOf(j) < k)
					letters[j.charCodeAt(0) - 97 + 26][k]++;
				else {
					letters[j.charCodeAt(0) - 97][k]++;
				}
			}
		}
	});

	storAllWords.forEach(function (element) {
		for (var k = 0; k < element.length; k++) {
			var j = element.charAt(k);
			if (element.indexOf(j) < k) {
				chars[j.charCodeAt(0) - 97 + 26] += 1;
			}
			else {
				chars[j.charCodeAt(0) - 97] += 1;
			}
		}
	});


	var rawFile2 = new XMLHttpRequest();
	rawFile2.open("GET", "sortedWords.txt", false);
	rawFile2.onreadystatechange = function () {
		if (rawFile2.readyState === 4) {
			if (rawFile2.status === 200 || rawFile2.status == 0) {
				var allText = rawFile2.responseText;
				allText.split(", ").forEach(function (element) {
					words.push(element);
				});
			}
		}
	}
	rawFile2.send(null);

	var rawFile3 = new XMLHttpRequest();
	rawFile3.open("GET", "freqList.txt", false);
	rawFile3.onreadystatechange = function () {
		if (rawFile3.readyState === 4) {
			if (rawFile3.status === 200 || rawFile3.status == 0) {
				var allText = rawFile3.responseText;
				allText.split(", ").forEach(function (element) {
					let temp = element.split("=");
					allWordFreq.set(temp[0], parseFloat(temp[1]));
				});
			}
		}
	}
	rawFile3.send(null);

	var sortedNumDesc = new Map([...allWordFreq].sort((a, b) => a[1] - b[1]));
	allWordFreq = sortedNumDesc;

	console.log(chars)
	console.log(letters)
}

async function recur(answer, isComp) {
	if (isComp) {
		theAnswer = answer.toLowerCase();
		if (getWords(getEligible(isComp, false)) == 1) {
			for (var i = 0; i < wordGuess.length; i++) {
				inLoop = true;
				var id = "3" + (wordCounter) + "" + i;
				document.getElementById(id).value = ("" + wordGuess.charAt(i)).toUpperCase();
				document.getElementById(id).style.backgroundColor = "#6aaa64";
				document.getElementById(id).style.color = "white";

				await new Promise(r => setTimeout(r, 20));

				for (var j = 0; j < 5; j++) {
					for (var k = wordCounter + 1; k < 6; k++) {
						document.getElementById("3" + k + j).style.backgroundColor = "white";
						document.getElementById("3" + k + j).value = "";
					}
				}

			} inLoop = false;

			document.getElementById("comp").innerHTML += " - " + (wordCounter + 1) + "/6";
			return wordCounter;
		}

		else if (wordCounter == 7) {
			document.getElementById("comp").innerHTML += " - X/6";
		}
		else {
			for (var i = 0; i < wordGuess.length; i++) {
				inLoop = true;
				var id = "3" + (wordCounter - 1) + "" + i;
				document.getElementById(id).value = ("" + wordGuess.charAt(i)).toUpperCase();
				if (colorResult.charAt(i) == 'G')
					document.getElementById(id).style.backgroundColor = "#6aaa64";
				if (colorResult.charAt(i) == 'Y')
					document.getElementById(id).style.backgroundColor = "#c9b458";
				if (colorResult.charAt(i) == 'B')
					document.getElementById(id).style.backgroundColor = "#787c7e";
				document.getElementById(id).style.color = "white";
				await new Promise(r => setTimeout(r, 15));
			}
			await new Promise(r => setTimeout(r, 80));
			inLoop = false;
			return recur(answer, true);
		}
	}
	else {
		colorResult = getColors(wordGuess);
		let prevArr = getEligible(isComp, false);
		var prevVal = getValue(answer, false);
		var otherVal = getValue(prevArr.entries().next().value[1]);

		console.log("mid: " + words.indexOf("prank") + " " + words.length)

		if (Math.round(prevVal) == 0) {
			prevVal = getValue(answer, true);
		}

		if (Math.round(otherVal) == 0) {
			reportleMultiplier = 0;
		}

		results = (getWords(answer));
		let arr = getEligible(isComp);


		var isFound = false;
		var indexFound = 0;
		prevArr.forEach(function (value, key) {
			if (value == answer) {
				isFound = true;
			}
			if (!isFound) {
				indexFound++;
			}
		});

		console.log(prevArr)

		if (wordCounter == 1) {
			if (indexFound > 4) {
				startingWord = true;
			}
		}

		var valid = true;
		yellowLetters.forEach(function (value, key) {
			if (!answer.toLowerCase().includes(key)) {
				valid = false;
			}
		});
		greenLetters.forEach(function (value, key) {
			if (!answer.toLowerCase().includes(key)) {
				valid = false;
			}
		});
		blackLetters.forEach(function (value, key) {
			if (answer.toLowerCase().includes(key)) {
				valid = false;
			}
		});

		console.log("Valid: " + valid)
		console.log("Is found: " + isFound)

		if (valid || isFound) {
			reportle += (prevVal / otherVal);
		}
		else {
			reportleMultiplier = 0;
		}

		console.log("Prev val: " + prevVal)
		console.log("Other val: " + otherVal)
		console.log("Reportle: " + reportle)

		console.log("User: " + answer)
		console.log("Comp: " + prevArr.entries().next().value[1]);

		setValues(answer, false);
		setValues(prevArr.entries().next().value[1], true);

		if (results == 1) {
			wordCounter++;
		}


		var wordGuess2 = answer;

		for (var i = 0; i < wordGuess2.length; i++) {
			var id = "1" + (wordCounter - 1) + "" + i;
			colorResult = getColors(wordGuess2);
			document.getElementById(id).innerHTML = ("" + wordGuess2.charAt(i)).toUpperCase();
			if (colorResult.charAt(i) == 'G')
				document.getElementById(id).style.backgroundColor = "#6aaa64";
			if (colorResult.charAt(i) == 'Y')
				document.getElementById(id).style.backgroundColor = "#c9b458";
			if (colorResult.charAt(i) == 'B')
				document.getElementById(id).style.backgroundColor = "#787c7e";
			document.getElementById(id).style.color = "white";
			document.getElementById(id).value = wordGuess2.toUpperCase().charAt(i);
		}

		if (results == 1) {
			for (var i = 0; i < 6; i++) {
				for (var j = 0; j < 5; j++) {
					if (i != 6) {
						document.getElementById("1" + i + j).setAttribute("readonly", true);
						document.getElementById("1" + i + j).removeAttribute("focusable");
						document.getElementById("1" + i + j).setAttribute("tabindex", -1);
					}
					else {
						document.getElementById("1" + i + j).removeAttribute("readonly");
						document.getElementById("1" + i + j).setAttribute("tabindex", 1);
						document.getElementById("1" + i + j).setAttribute("focusable", true);
					}
				}
			}
			document.getElementById("messages1").innerHTML = "You guessed '" + theAnswer.toUpperCase() + "' in " + wordCounter + " tries! <br> Total Possible Final Guesses: " + prevArrSize;
			if (reportleMultiplier == 0) {
				document.getElementById("messages1").innerHTML += "<br>This grid is not eligible for a Reportle";
			}
			else {
				document.getElementById("messages1").innerHTML += ("<br>Your Reportle: " + (reportleMultiplier * Math.round((reportle / wordCounter) * 100)) + "%");
			}
			console.log("Word freq comp: " + allWordFrequencyComp + " Word freq user: " + allWordFrequencyUser)
			console.log("Letter freq comp: " + allLetterFrequencyComp + " Letter freq user: " + allLetterFrequencyUser)
			console.log("Pos comp: " + allPositionComp + "Pos user: " + allPositionUser)
			giveFeedback();
			document.getElementById("yours").innerHTML = "Your Solution - " + wordCounter + "/6";
			document.getElementById("spacer0").style.width = "105px";
			document.getElementById("spacer1").style.width = "70px";
			document.getElementById("spacer2").style.width = "60px";
			if (document.getElementById("solve").disabled == false) {
				document.getElementById("solve").click();
			} else {
				document.getElementById("clear").focus();
			}
		}

		var index = 0;
		var wordGuesses = [];
		if (results != 1) {
			arr.forEach(async function (value, key) {
				index++;
				if (index <= 6) {
					wordGuesses.push(value);
				}
			});
			for (var j = 0; j < wordGuesses.length; j++) {
				inLoop = true;
				var wordGuess2 = wordGuesses[j];
				for (var i = 0; i < wordGuess2.length; i++) {
					var id = "2" + (j) + "" + i;
					colorResult = getColors(wordGuess2);
					document.getElementById(id).value = ("" + wordGuess2.charAt(i)).toUpperCase();
					if (colorResult.charAt(i) == 'G')
						document.getElementById(id).style.backgroundColor = "#6aaa64";
					if (colorResult.charAt(i) == 'Y')
						document.getElementById(id).style.backgroundColor = "#c9b458";
					if (colorResult.charAt(i) == 'B')
						document.getElementById(id).style.backgroundColor = "#787c7e";
					document.getElementById(id).style.color = "white";
					await new Promise(r => setTimeout(r, 15));
				}
				await new Promise(r => setTimeout(r, 30));
			}
			inLoop = false;
			if (index <= 5) {
				for (var i = index; i <= 5; i++) {
					inLoop = true;
					for (var j = 0; j < 5; j++) {
						document.getElementById("2" + i + j).style.backgroundColor = "white";
						document.getElementById("2" + i + j).value = "";
						await new Promise(r => setTimeout(r, 15));
					}
					await new Promise(r => setTimeout(r, 30));
				}
				inLoop = false;
			}

			if (wordCounter >= 6) {
				document.getElementById("messages1").innerHTML = "You weren't able to guess '" + theAnswer.toUpperCase() + "'";
			}
			else {
				document.getElementById("messages1").innerHTML = "Enter each of your guesses";
			}
			document.getElementById("messages1").innerHTML += "<br> Total Possible Guesses: " + arr.size;
			prevArrSize = arr.size;
			if (reportleMultiplier == 0) {
				document.getElementById("messages1").innerHTML += "<br>This grid is not eligible for a Reportle";
			}
			else {
				document.getElementById("messages1").innerHTML += ("<br>Your Reportle: " + (reportleMultiplier * Math.round((reportle / wordCounter) * 100)) + "%");
			}
			console.log("Word freq comp: " + allWordFrequencyComp + " Word freq user: " + allWordFrequencyUser)
			console.log("Letter freq comp: " + allLetterFrequencyComp + " Letter freq user: " + allLetterFrequencyUser)
			console.log("Pos comp: " + allPositionComp + " Pos user: " + allPositionUser)
		}
	}
}

function giveFeedback() {
	var feedback = "FEEDBACK: ";

	var wordFreqScore = (allWordFrequencyUser);
	var letterFreqScore = (Math.round((allLetterFrequencyUser / allLetterFrequencyComp) * 100));
	var positionScore = (Math.round((allPositionUser / allPositionComp) * 100));

	console.log("WF: " + wordFreqScore)
	console.log("LF: " + letterFreqScore)
	console.log("PS: " + positionScore)

	// document.getElementById("letterpos").addEventListener('click', function () {
	// 	chrome.tabs.create({ url: this.href });
	// }, true);

	if (!startingWord && wordFreqScore >= allWordFrequencyComp && letterFreqScore == 100 && positionScore == 100) {
		document.getElementById("messages3").innerHTML = (feedback + "You played perfectly!");
	}
	else {
		if (startingWord) {
			feedback += "<br>Use a starting word that is more statistically helpful, such as ROATE or SOARE";
		}
		if (wordFreqScore < allWordFrequencyComp) {
			feedback += "<br>Guess words that are most common, since these are more likely to be the Wordle solution";
		}
		if (letterFreqScore > positionScore) {
			feedback += "<br>Guess letters in more common orders. For more: <a href='https://docs.google.com/document/d/1DZ3qRC8hxkfQ4lT4f1GNK1Z_yYQ8yP7f4nMDhF-sdOU/edit?usp=sharing'>Click here</a>";
		}
		if (positionScore > letterFreqScore) {
			feedback += "<br>Guess letters that are more frequent. For more: <a href='https://docs.google.com/document/d/1r0CRRWI8rFYPxTPHyfrhANjXn6Zms_cU-2hVMUcbKsU/edit?usp=sharing'>Click here</a>";
		}
		document.getElementById("messages3").innerHTML = feedback;
	}


}

function getWords(word) {
	let wordGrid = [];

	wordGuess = String(word);

	for (var i = 0; i < 5; i++) {
		wordGrid.push(wordGuess.charAt(i));
	}
	colorResult = String(getColors(wordGuess));
	if (colorResult === "GGGGG") {
		return 1;
	}
	for (var i = 0; i < 5; i++) {
		if (colorResult.charAt(i) == 'G') {
			if (greenLetters.has(wordGrid[i])) {
				greenLetters.get(wordGrid[i]).push(i);
			} else {
				var temp = [];
				temp.push(i);
				greenLetters.set(wordGrid[i], temp);
			}
		} else if (colorResult.charAt(i) == 'Y') {
			if (yellowLetters.has(wordGrid[i])) {
				yellowLetters.get(wordGrid[i]).push(i);
			} else {
				var temp = [];
				temp.push(i);
				yellowLetters.set(wordGrid[i], temp);
			}
		} else if (colorResult.charAt(i) == 'B') {
			if (!blackLetters.has(wordGrid[i])) {
				blackLetters.set(wordGrid[i],
					count(wordGuess, wordGrid[i]) - countBlack(wordGuess, colorResult, wordGrid[i]));
			}

		}

	}

	wordCounter++;
	return 0;
}

function getColors(word) {
	const result = ['H', 'H', 'H', 'H', 'H'];

	const answer = [theAnswer.charAt(0), theAnswer.charAt(1), theAnswer.charAt(2), theAnswer.charAt(3),
	theAnswer.charAt(4)];

	for (var i = 0; i < 5; i++) {
		if (String(word).charAt(i) == answer[i]) {
			result[i] = 'G';
			answer[i] = ' ';
		}
	}
	for (var i = 0; i < 5; i++) {
		if (result[i] != 'G') {
			var found = false;
			var index = 0;
			for (var j = 0; j < 5; j++) {
				if (answer[j] == String(word).charAt(i)) {
					found = true;
					index = j;
				}
			}
			if (found) {
				result[i] = 'Y';
				answer[index] = ' ';
			} else {
				result[i] = 'B';
			}
		}
	}

	return "" + result[0] + result[1] + result[2] + result[3] + result[4];
}

function getEligible(isComp, zeroFreq) {
	eligibleWords.clear();
	for (var i = 0; i < allWords.length; i++) {
		var word = allWords.at(i).toLowerCase();
		var greenCheck = true;
		greenLetters.forEach(function (value, key) {
			value.forEach(function (element) {
				if (word.charAt(element) != key) {
					greenCheck = false;
				}
			});
		});

		if (greenCheck) {
			var yellowCheck = true;
			yellowLetters.forEach(function (value, key) {
				if (blackLetters.has(key) && count(word, key) != count(wordGuess, key) - countBlack(wordGuess, colorResult, key)) {
					yellowCheck = false;
				}
				else if (!blackLetters.has(key) && count(word, key) < count(wordGuess, key) - countBlack(wordGuess, colorResult, key)) {
					yellowCheck = false;
				}
				value.forEach(function (element) {
					if (word.charAt(element) == key) {
						yellowCheck = false;
					}
				});
			});


			if (yellowCheck) {
				var blackCheck = true;

				blackLetters.forEach(function (value, key) {
					if (count(word, key) > value) {
						blackCheck = false;
					}
				});
				if (blackCheck) {
					var value = getValue(word, zeroFreq);
					while (eligibleWords.has(value)) {
						value -= 0.000001;
					}
					eligibleWords.set(value, word);
				}
			}
		}
	}
	const sortedNumDesc = new Map([...eligibleWords].sort((a, b) => a[0] - b[0]));
	var tempEligWords = sortedNumDesc.entries();
	if (isComp)
		return tempEligWords.next().value[1];
	else
		return sortedNumDesc;
}

function getValue(word, zeroFreq) {
	var totalVariety = 0;
	var totalPosition = 0;
	var totalFrequency = 1;
	for (var j = 0; j < word.length; j++) {
		if (!greenLetters.has(word.charAt(j)) && !yellowLetters.has(word.charAt(j))) {
			if (word.charCodeAt(j) > 96) {
				totalPosition += letters[word.charCodeAt(j) - 97][j];
				if (word.indexOf(word.charAt(j)) == j)
					totalVariety += chars[word.charCodeAt(j) - 97];
				else
					totalVariety += chars[word.charCodeAt(j) - 97 + 26];
			}
		}
		else if (!blackLetters.has(word.charAt(j))
			&& count(word, word.charAt(j)) > countYellow(wordGuess, colorResult, word.charAt(j))
			+ countGreen(wordGuess, colorResult, word.charAt(j))) {
			var numCounted = countYellow(wordGuess, colorResult, word.charAt(j))
				+ countGreen(wordGuess, colorResult,
					word.charAt(j));
			var tempCount = 0;
			for (var k = 0; k < j; k++) {
				if (word.charAt(k) == word.charAt(j))
					tempCount++;
			}
			if (tempCount >= numCounted) {
				if (word.charCodeAt(j) > 96) {
					totalPosition += letters[word.charCodeAt(j) - 97][j];
					if (word.indexOf(word.charAt(j)) == j)
						totalVariety += chars[word.charCodeAt(j) - 97];
					else
						totalVariety += chars[word.charCodeAt(j) - 97 + 26];
				}
			}
		}
	}
	if (wordCounter > 0 && !zeroFreq) {
		totalFrequency = getFrequency(word);
	}
	if (zeroFreq) {
		totalFrequency = 0.5;
	}
	var tempWordCounter = wordCounter + 0.1;
	return Math.floor(-(totalVariety * ((6 - tempWordCounter) / 2.0) + totalPosition * (tempWordCounter * 2.5)) * Math.pow(totalFrequency, 3));

}

function setValues(word, isComp, zeroFreq) {
	var totalVariety = 0;
	var totalPosition = 0;
	var totalFrequency = 1;
	for (var j = 0; j < word.length; j++) {
		if (!greenLetters.has(word.charAt(j)) && !yellowLetters.has(word.charAt(j))) {
			if (word.charCodeAt(j) > 96) {
				totalPosition += letters[word.charCodeAt(j) - 97][j];
				if (word.indexOf(word.charAt(j)) == j)
					totalVariety += chars[word.charCodeAt(j) - 97];
				else
					totalVariety += chars[word.charCodeAt(j) - 97 + 26];
			}
		}
		else if (!blackLetters.has(word.charAt(j))
			&& count(word, word.charAt(j)) > countYellow(wordGuess, colorResult, word.charAt(j))
			+ countGreen(wordGuess, colorResult, word.charAt(j))) {
			var numCounted = countYellow(wordGuess, colorResult, word.charAt(j))
				+ countGreen(wordGuess, colorResult,
					word.charAt(j));
			var tempCount = 0;
			for (var k = 0; k < j; k++) {
				if (word.charAt(k) == word.charAt(j))
					tempCount++;
			}
			if (tempCount >= numCounted) {
				if (word.charCodeAt(j) > 96) {
					totalPosition += letters[word.charCodeAt(j) - 97][j];
					if (word.indexOf(word.charAt(j)) == j)
						totalVariety += chars[word.charCodeAt(j) - 97];
					else
						totalVariety += chars[word.charCodeAt(j) - 97 + 26];
				}
			}
		}
	}
	totalFrequency = getFrequency(word);
	if (wordCounter == 1) {
		totalFrequency = 1;
	}
	if (zeroFreq) {
		totalFrequency = 0.1;
	}

	if (isComp) {
		if (totalFrequency != 0) {
			allWordFrequencyComp += totalFrequency;
		}
		allLetterFrequencyComp += totalVariety;
		allPositionComp += totalPosition;
	}
	else {
		if (totalFrequency != 0) {
			allWordFrequencyUser += totalFrequency;
		}
		allLetterFrequencyUser += totalVariety;
		allPositionUser += totalPosition;
	}
}

function count(word, letter) {
	var count = 0;
	for (var i = 0; i < word.length; i++) {
		if (word.charAt(i) == letter)
			count++;
	}
	return count;
}

function countBlack(word, colors, character) {
	var count = 0;
	for (var i = 0; i < word.length; i++) {
		if (word.charAt(i) == character && colors.charAt(i) == 'B') {
			count++;
		}
	}
	return count;
}

function countYellow(yWord, colors, character) {
	var count = 0;
	for (var i = 0; i < yWord.length; i++) {
		if (yWord.charAt(i) == character && wordCounter > 0 && colors.charAt(i) == 'Y') {
			count++;
		}
	}

	return count;
}

function countGreen(yWord, colors, character) {
	var count = 0;
	for (var i = 0; i < yWord.length; i++) {
		if (yWord.charAt(i) == character && wordCounter > 0 && colors.charAt(i) == 'G') {
			count++;
		}
	}

	return count;
}

function countAllBlack(word, colors) {
	var count = 0;
	for (var i = 0; i < word.length; i++) {
		if (colors.charAt(i) == 'B') {
			count++;
		}
	}
	return count;
}

function getFrequency(word) {
	var xVal = 0;
	if (wordCounter > 4) {
		xVal = words.indexOf(word) - 3000;
	}
	else {
		xVal = words.indexOf(word) - 5000;
	}
	if (countAllBlack(wordGuess, colorResult) < 2 && wordCounter > 4) {
		xVal -= 2000;
	}
	return (1.0 / (1.0 + Math.pow(Math.E, -xVal)));
}
