# hsresumebuilder

## Language levels
For the language levels, please categorize them from 0, lowest, to 4, highest levels of proficiency.

0 : limited proficiency
1 : limited proficiency +
2 : high proficiency
3 : almost native
4 : native


## Create your own

See my sample YAML that I use for creating my own CV:

```yaml
---
appearancePreferences:
  theme: anachronic
  documentTitles:
    shortIntroSectionTitle: "WHO AM I?"
    workExperienceSectionTitle: "WORK EXPERIENCE"
    educationSectionTitle: "EDUCATION"
    interestsHobbiesInformationTitle: "MY INTERESTS"
    driverLicenseInformationTitle: "DRIVING"
    languagesInformationTitle: "LANGUAGES"
    seeMyWebsitesSectionTitle: "ME ON THE INTERNET"

personalInformation:
  addressLines:
    - "Hugo de Grootstraat 19"
    - "1441KH Purmerend"
    - "The Netherlands"
  contactInformation:
    emails:
      - "E-mail: jjbigorra@gmail.com"
    phoneNumbers:
      - "Phone: +31 639825138"
    websites:
      - "averageflow.github.io"
      - "github.com/averageflow"
  displayName: "Josep Bigorra"
  jobTitle: "Software Engineer"
  shortIntro:
    - "I am a Software Engineer with DevOps experience that is always eager to learn while perfecting my skills and projects and taking on challenges. Living in The Netherlands since 2019, born 23/12/1996 in Spain."
    - "I love to create solutions, improving, automating workflows, creating fast and reliable programs, helping and teaching others and to express myself via my creations."
    - "I love the Ops side of the projects and platforms too. I am a UNIX guy and can work comfortably with Linux, BSDs and macOS, in that order of preference. I like distributed tracing, infrastructure-as-code, CI/CD, and type-safe languages."

workExperienceInformation:
  - entityName: "IKEA, Amsterdam, Netherlands"
    experiencePoints:
      - "Experience in Java, Spring Boot and Gradle"
      - "Apache Beam, Dataflow, data-pipelines"
      - "Google Cloud Platform, Pub/Sub"
      - "Terraform, Infrastructure-as-code"
      - "Kubernetes and Docker"
      - "PostgreSQL, BigTable, ElasticSearch"
      - "Lots of unit and integration testing, mocking"
    positionName: "Software Engineer"
    timeWorked: "January 2022 - Present"
  - entityName: "Pro Warehouse, Amsterdam, Netherlands"
    experiencePoints:
      - "Extensive experience with Go (standard & Gin) and PHP (Laravel)."
      - "Relational MySQL-based applications, with complex queries and multiple joins."
      - "Database design, normalization, migration, maintenance, and backup."
      - "REST API design, implementation and documentation generation."
      - "Server-side rendering frontend applications, communicating with several APIs."
      - "Secure-by-design practices."
      - "Jenkins Groovy scripted pipelines with unit tests."
      - "FreeBSD & Linux server experience, jails and pf Firewall."
    positionName: "Lead Software Engineer, Scrum Master"
    timeWorked: "November 2019 - December 2021"
  - entityName: "Liderlink Business Solutions, Parchal, Portugal"
    experiencePoints:
      - "Several projects with PHP and MySQL, CRUD operations, in several real-estate, hotel and personnel management software."
      - "Client-side scripting with JavaScript and jQuery."
      - "Android development."
      - "Ubuntu server experience."
    positionName: "Software Developer Internship"
    timeWorked: "February 2019 to July 2019"
  - entityName: "Freelancer.com.br, Online Remote"
    experiencePoints: []
    positionName: "CMS Admin & Customer Support"
    timeWorked: "June 2018 to January 2019"

educationInformation:
  - entityName: "UAlg, Faro, Portugal"
    experiencePoints:
      - "Graduated with 190/200 score. Maximum score for the software developer internship."
    positionName: "Information Systems & Technologies degree"
    timeWorked: "September 2017 to September 2019"

languagesInformation:
  simpleMode: false
  simpleModeContent: "test"
  complexModeContent:
    - languageName: "Catalan"
      speakingProficiency: 4
      writingProficiency: 4
      readingProficiency: 4
    - languageName: "Spanish"
      speakingProficiency: 4
      writingProficiency: 4
      readingProficiency: 4
    - languageName: "Portuguese"
      speakingProficiency: 4
      writingProficiency: 4
      readingProficiency: 4
    - languageName: "English"
      speakingProficiency: 3
      writingProficiency: 3
      readingProficiency: 3
    - languageName: "Dutch"
      speakingProficiency: 2
      writingProficiency: 2
      readingProficiency: 2

driverLicenseInformation:
  - "B driver's license, for (automobile) vehicles up to 3,5 ton"

interestsHobbiesInformation:
  - "I love playing guitar and creating some loops in Logic Pro. I have a secret passion for building computers and recycling them, as well as modding operating systems and tinkering with bootloaders, making my own servers and playing with FreeBSD and other Linux distributions."
  - "I like to blog about my tech opinions and like to play strategy games sometimes."
  - "I love natural wood and stone and you might find me sanding furniture down, oiling wood, or removing weeds from the garden."

```