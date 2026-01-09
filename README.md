# Madrigal EHR System - R Shiny

A basic electronic health records system built in R Shiny for outpatient and telemedicine practice management.

## Features

### Current Functionality

**Patient Management**
- Create and manage patient records with auto-generated MRNs
- Search patient database
- Edit patient demographics (name, date of birth)

**Clinical Documentation**
- SOAP note format (Subjective, Objective, Assessment/Plan)
- Chief complaint tracking
- Visit history with date stamps
- ICD-10 diagnosis coding with searchable database
- CPT procedure coding (in development)

**Sidebar Clinical Lists**
- Problem list with add/remove functionality
- Medication list management
- Allergy tracking with visual highlighting

**Vitals Tracking**
- Weight (kg)
- Height (cm)
- Waist circumference (cm)
- Automatic BMI calculation
- Historical vitals display in table format

**Data Management**
- Export patient charts as JSON files
- All data persists during session

## Installation

### Required R Packages

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "DT",
  "jsonlite",
  "readxl",
  "digest"
))
```

### Required Data Files

Place these files in your R working directory:
- `ICD Codes 2025.xlsx` - ICD-10 codes from CMS with columns: CODE, SHORT DESCRIPTION
- `CPT Codes 2026.xlsx` - CPT codes from CMS with columns: CODE, SHORT DESCRIPTION

The application will function with sample codes if these files are not present, but full functionality requires the complete code sets.

## Data Storage and Security

### Encrypted Local Storage

Patient data is automatically saved to an encrypted file (`ehr_data.rds`) after each change. The encryption uses XOR cipher with a SHA-256 key stored in `.ehr_key`.

**Important Security Notes:**
- The `.ehr_key` file is generated automatically on first run
- Keep `.ehr_key` secure and backed up - without it, your data cannot be decrypted
- Add `.ehr_key` to your `.gitignore` to prevent accidentally committing it
- The encryption provides basic obfuscation but is not military-grade
- For production use, implement proper encryption (AES-256) and key management

### Data Files

The application creates these files:
- `ehr_data.rds` - Encrypted patient data
- `.ehr_key` - Encryption key (keep secure)

### Backup Recommendations

Regular backups are essential:
```bash
# Backup both the data file and key
cp ehr_data.rds ehr_data_backup_$(date +%Y%m%d).rds
cp .ehr_key .ehr_key_backup
```

## Usage

### Starting the Application

```r
# Run from R or RStudio
shiny::runApp()
```

### Basic Workflow

1. **Create a Patient**: Click "New Patient" button and enter demographics
2. **Add Clinical Information**: Use sidebar to add problems, medications, and allergies
3. **Document a Visit**: Navigate to the Visits tab and click "New Visit"
   - Enter chief complaint
   - Document SOAP note sections
   - Search and select ICD-10 codes
   - Save visit
4. **Record Vitals**: Navigate to Vitals tab, enter measurements, and click "Add Vitals"
5. **Export Chart**: Use "Export Chart" button to download patient data as JSON

### ICD-10 Code Search

The ICD-10 search is case-insensitive and searches both code and description fields. Examples:
- Search "diabetes" to find all diabetes-related codes
- Search "E11" to find specific code ranges
- Search returns top 100 matches for performance

## Data Structure

Patient data is stored in reactive dataframes with the following structure:

**Patients**: id, name, dob, mrn
**Problems**: patient_id, id, text
**Medications**: patient_id, id, text
**Allergies**: patient_id, id, text
**Visits**: patient_id, id, date, chief_complaint, subjective, objective, assessment_plan, icd10_codes, cpt_codes
**Vitals**: patient_id, id, date, weight, height, waist, bmi

## Planned Features

### Near-term Development
- Digital consent forms and intake questionnaires
- Calendly integration for appointment scheduling
- Enhanced chart export with PDF formatting
- CPT code search functionality (currently in development)

### Future Enhancements
- Database backend for persistent storage
- User authentication and role-based access
- E-prescribing integration
- Lab results interface
- Billing and claims submission
- HIPAA compliance audit logs

## Architecture Notes

The application uses Shiny's reactive programming model with reactiveVal() for state management. This architecture supports:
- Real-time updates across UI components
- Easy integration with external APIs
- Scalable data structure for adding new features
- Future migration to database backend

## Known Issues

- CPT code loading and search functionality needs troubleshooting
- XOR encryption provides basic security but should be upgraded to AES-256 for production
- No user authentication or access controls
- Limited to single-user operation
- No automated backup system

## Contributing

This is a basic framework designed to be extended. Key areas for contribution:
- Database integration (PostgreSQL, MySQL, SQLite)
- PDF generation for chart exports
- Additional clinical templates
- Telemedicine video integration
- HL7/FHIR interoperability

## License

To be determined.

## Disclaimer

This software is for educational and development purposes. It is not intended for production use in clinical settings without proper validation, security implementation, and HIPAA compliance measures.