/*
References:
- https://www.bpwebs.com/crud-operations-on-google-sheets-with-online-forms/
- https://yagisanatode.com/2020/11/25/creating-an-embedded-interactive-chain-story-app-with-google-apps-script-and-google-sheets/
- https://developers.google.com/apps-script/guides/html/best-practices
*/

// --- Global variables ----------------------------------------------------

const PAGE_TITLE = "CCMF Race Relations Data";

const SCRIPT_LOCK = LockService.getScriptLock();

const SS_TABLES = {
  main: new Table("Data", "DataHistory"),
  categories: new Table("Categories"),
  locations: new Table("Locations"),
};

// --- Web app UI ----------------------------------------------------------

// Open the form as a web app
function doGet(e) {
  return HtmlService.createTemplateFromFile("Index")
    .evaluate()
    .setTitle(PAGE_TITLE)
    .addMetaTag("viewport", "width=device-width, initial-scale=1");
}

// --- Modial dialog UI ----------------------------------------------------

// Open the form in model dialog (handy during development, don't have to
// keep redeploying web app to see changes)
function showFormInModalDialog() {
  var form = HtmlService.createTemplateFromFile("Index").evaluate();
  form.setWidth(800).setHeight(600);
  SpreadsheetApp.getUi().showModalDialog(form, PAGE_TITLE);
}

// Create custom menu for modal dialog
function onOpen() {
  SpreadsheetApp.getUi()
    .createMenu("Testing")
    .addItem("Modal Dialog Form", "showFormInModalDialog")
    .addToUi();
}

// --- HTML helper function -----------------------------------------------

// Include CSS, JS, etc. content from html files
function include(filename) {
  return HtmlService.createHtmlOutputFromFile(filename).getContent();
}

// --- General helper functions --------------------------------------------

function nowTimestamp() {
  return Utilities.formatDate(new Date(), "GMT-8", "yyyy-MM-dd HH:mm:ss");
}

// Filtering function for getting the unique values in an array
function onlyUnique(value, index, self) {
  return self.indexOf(value) === index;
}

// Helper function - return a string of comma-separated values if input
// is an array, otherwise return the original input
function commaSeparatedString(inputVal) {
  var outputVal = inputVal;
  if (Array.isArray(outputVal)) {
    outputVal = outputVal.join(", ");
  }
  return outputVal;
}

// === SPREADSHEET FUNCTIONS ================================================

// --- Spreadsheet helper functions ---------------------------------------

// Throw a custom error when a spreadsheet operation failed
function throwSheetError(message) {
  throw "Spreadsheet Error: " + message;
}

function getSheet(ss, sheetName) {
  Logger.log("Getting sheet: " + sheetName);
  sheet = ss.getSheetByName(sheetName);
  if (!sheet) {
    throwSheetError("Sheet '" + sheetName + "' not found.");
  }
  Logger.log("Success!");
  return sheet;
}

// While using spreadsheet, use script lock to prevent others from modifying
// the spreadsheet
function scriptLock(timeoutInMillis = 30000) {
  // Logger.log("Starting function scriptLock()");

  // Temporary
  // Logger.log("Initial script lock status: " + SCRIPT_LOCK.hasLock());

  // Throws an error if it can't get a lock
  SCRIPT_LOCK.waitLock(timeoutInMillis);

  // Temporary
  // Logger.log("Final script lock status: " + SCRIPT_LOCK.hasLock());
}

// Unlock script
function scriptUnlock() {
  // Logger.log("Starting function scriptUnlock().");

  // Temporary
  // Logger.log("Initial script lock status: " + SCRIPT_LOCK.hasLock());

  // Ensure all pending processes to the sheet are completed
  Logger.log("Flushing spreadsheet.");
  SpreadsheetApp.flush();

  // Unlock script
  SCRIPT_LOCK.releaseLock();

  // Temporary
  // Logger.log("Final script lock status: " + SCRIPT_LOCK.hasLock());
}

// --- Custom Table() object for each data table in spreadsheet ------------

function Table(sheetName, historySheetName = null) {
  // For a standalone script, use SpreadsheetApp.openById()
  this.ss = SpreadsheetApp.getActive();

  // Main data sheet for this table
  this.sheet = getSheet(this.ss, sheetName);

  // Optional sheet with history data for this table
  if (historySheetName) {
    this.historySheet = getSheet(this.ss, historySheetName);
  } else {
    this.historySheet = null;
  }

  // Get data (all or selected range in A1 notation)
  this.getData = function (a1Notation = null, history = false, unlock = true) {
    Logger.log("Starting function getData().");

    let sheet = this.sheet;
    if (history) {
      sheet = this.historySheet;
    }

    // Lock script while accessing sheet
    scriptLock();

    // Get data from sheet
    let values = null;
    if (a1Notation) {
      // Get selected range
      values = sheet.getRange(a1Notation).getValues();
    } else {
      // Get all data
      values = sheet.getDataRange().getValues();
    }
    if (!values) {
      throwSheetError("Unable to get data from sheet " + sheet.getName());
    }
    Logger.log("Got data from sheet " + sheet.getName());

    // Unlock
    if (unlock) {
      scriptUnlock();
    }

    // Bundle the output into an object with labels for columns
    let data = {
      labels: values[0],
      values: values.slice(1),
    };

    // Logger.log(JSON.stringify(data, null, 2));

    return data;
  };

  // Get all record IDs
  this.getRecordIds = function (unlock = true) {
    Logger.log("Starting function getRecordIds().");

    let recordIds = {};

    if (this.historySheet) {
      recordIds.historyIds = this.getData(
        (a1Notation = "A:A"),
        (history = true),
        (unlock = false)
      )
        .values.map((x) => x[0])
        .filter((x) => x);
    } else {
      recordIds.historyIds = [];
    }

    recordIds.ids = this.getData(
      (a1Notation = "A:A"),
      (history = false),
      (unlock = unlock)
    )
      .values.map((x) => x[0])
      .filter((x) => x);

    return recordIds;
  };

  // Create new record
  this.createRecord = function (values) {
    Logger.log("Starting function createRecord().");

    // Get all record IDs and keep script locked
    let recordIds = this.getRecordIds((unlock = false));
    let idsAll = recordIds["ids"].concat(recordIds["historyIds"]);
    // Logger.log("All record IDs:");
    // Logger.log(idsAll);

    // Assign a new ID for this record
    values[0] = Math.max.apply(Math, idsAll) + 1;

    // Assign timestamp
    values[1] = nowTimestamp();

    // Apend new row and unlock script
    Logger.log("Appending row to " + this.sheet.getName());
    try {
      this.sheet.appendRow(values);
      Logger.log("Success!");
    } catch (err) {
      throwSheetError("Unable to append row. Details: " + err);
    }
    scriptUnlock();

    return values;
  };
}

// === FORM SETUP ==========================================================

// --- Form setup: helper functions ----------------------------------------

function cleanFieldName(str) {
  return str
    .replace("Gender of Victim(s)", "gender")
    .replace("Type of Incident", "incident-type")
    .toLowerCase()
    .replace(" ", "-");
}

function getCategories(excludeFlagged = true) {
  Logger.log("Starting function getCategories().");
  let categoriesAll = SS_TABLES["categories"].getData().values;

  // Filter out any values that have been flagged to exclude from the form
  if (excludeFlagged) {
    categoriesAll = categoriesAll.filter((x) => x[6] != "y");
  }

  // Array of unique fields
  let fields = categoriesAll.map((x) => x[1]).filter(onlyUnique);
  let fieldLabel = null;
  let categoriesFiltered = [];
  let categoriesMain = [];
  let categoriesSub = [];
  let categoriesOut = {};

  for (let i = 0; i < fields.length; i++) {
    fieldLabel = cleanFieldName(fields[i]);
    categoriesFiltered = categoriesAll.filter((x) => x[1] == fields[i]);

    if (fieldLabel == "ethnic-community") {
      // Split into option groups
      categoriesMain = {
        Communities: categoriesFiltered
          .map((x) => x[2])
          .filter((x) => x != "N/A")
          .filter(onlyUnique)
          .sort(),
      };

      categoriesSub = {
        "Sub-Communities": categoriesFiltered
          .filter((x) => x[3])
          .map((x) => x[4])
          .filter((x) => x != "N/A")
          .sort(),
      };

      categoriesOut[fieldLabel] = ["N/A", categoriesMain, categoriesSub];
    } else {
      // Single array of options
      categoriesOut[fieldLabel] = categoriesFiltered.map((x) => x[4]).sort();
    }
  }
  Logger.log("Success!");
  // Logger.log(JSON.stringify(categoriesOut, null, 2));

  return categoriesOut;
}

function getLocations() {
  Logger.log("Starting function getLocations().");
  let locationsAll = SS_TABLES["locations"].getData().values;
  let locationsProv = locationsAll.filter((x) => x[1] == "province");
  let provNames = locationsProv.map((x) => x[2]);
  let provCodes = locationsProv.map((x) => x[3]);

  // Object for looking up province names from codes
  let provLookup = {};
  for (let i = 0; i < provCodes.length; i++) {
    provLookup[provCodes[i]] = provNames[i];
  }

  // Initialize with provinces and N/A option
  let locationsOptions = ["N/A", { Provinces: provNames.sort() }];

  // Add municipalities for each province
  let provCode = null;
  let groupName = null;
  let municipalities = {};

  for (let j = 0; j < provCodes.length; j++) {
    provCode = provCodes[j];
    groupName = "Municipalities: " + provNames[j];
    municipalities = {};
    municipalities[groupName] = locationsAll
      .filter((x) => x[1] == "municipality" && x[3] == provCode)
      .map((x) => x[2])
      .sort();

    locationsOptions.push(municipalities);
  }

  let locationsOut = {
    "location-name": locationsOptions,
  };

  Logger.log("Success!");
  // Logger.log(JSON.stringify(locationsOut, null, 2));

  return locationsOut;
}

// --- Form setup: function used by Client.js -----------------------------

// Get values to populate main form options
function getMainFormOptions() {
  Logger.log("Starting function getMainFormOptions().");
  let options = getCategories();
  options["location-name"] = getLocations()["location-name"];
  // Logger.log(options);
  return options;
}

// === FORM PROCESSING =====================================================

// --- Form processing: helper functions -----------------------------------

// Get values from form and return as a 1-d array
function getMainFormData(formObject) {
  const provinces = {
    Alberta: "AB",
    "British Columbia": "BC",
    Manitoba: "MB",
    "New Brunswick": "NB",
    "Newfoundland and Labrador": "NL",
    "Northwest Territories": "NT",
    "Nova Scotia": "NS",
    Nunavut: "NU",
    Ontario: "ON",
    "Prince Edward Island": "PE",
    Quebec: "QC",
    Saskatchewan: "SK",
    Yukon: "YT",
  };

  let location = formObject["location-name"];

  // Parse location name into municipality and province
  // First check if the location name is a province
  let province = provinces[location];
  let municipality = null;
  if (!province) {
    // Try to parse the location into municipality, province
    let result = location.match(/(.*), ([A-Z][A-Z])/);
    if (result) {
      municipality = result[1];
      province = result[2];
    } else {
      // No match, set province equal to location (which will be N/A)
      province = location;
    }
  }

  const labels = [
    "incident-id",
    "timestamp",
    "article-url",
    "description",
    "pub-date",
    "incident-year",
    "incident-month",
    "incident-day",
    "location-name",
    "province",
    "municipality",
    "location-notes",
    "incident-category",
    "incident-type",
    "context",
    "ethnic-community",
    "indigenous-nation",
    "identity-based",
    "other-trends",
    "gender",
    "victim-name",
    "perp-name",
    "general-notes",
  ];

  // Assign array of the record values in the proper order
  let values = [];
  for (let i = 0; i < labels.length; i++) {
    label = labels[i];

    if (label == "province") {
      values.push(province);
    } else if (label == "municipality") {
      values.push(municipality);
    } else {
      values.push(commaSeparatedString(formObject[label]));
    }
  }

  // Bundle the values and labels into an object
  let record = {
    labels: labels,
    values: values,
  };

  return record;
}

// --- Form processing: function used by Client.js ----------------------------

// Process main form and return record object to client
function processMainForm(formObject) {
  const recordRaw = getMainFormData(formObject);
  let incidentIdRaw = recordRaw.values[0];
  let values = null;

  if (!incidentIdRaw) {
    // Incident ID is blank in the form, so create a new record
    values = SS_TABLES["main"].createRecord(recordRaw.values);
  }

  let record = { labels: recordRaw.labels, values: values };

  // Logger.log(formObject);
  Logger.log(record);

  return record;
}
