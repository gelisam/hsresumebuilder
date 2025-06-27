# Agent Instructions for hsresumebuilder

This document provides guidance for AI agents working with the `hsresumebuilder` Haskell project.

## Branch name

When choosing a branch name, please use a prefix which identifies you, the AI agent. For example, Google Jules should use branches which start with `jules/`, GitHub Copilot should use branches which start with `copilot/`, etc.

## Key Files and Their Purpose:

1.  **`hsresumebuilder.yaml`**:
    *   **Purpose**: This is the primary data file containing the resume content. It's written in YAML format and defines all sections of the resume, such as personal information, work experience, education, etc.
    *   **Agent Note**: When asked to modify resume content (e.g., add a new job, change a description, or update personal details), this is the file you will most likely need to edit. Pay close attention to the YAML structure, especially for optional fields and lists. Ensure data types match the Haskell definitions in `src/ResumeBuilder/ResumeBuilderModel.hs`.

2.  **`src/ResumeBuilder/ResumeBuilderModel.hs`**:
    *   **Purpose**: Defines the Haskell data types that represent the structure of the resume. These types (e.g., `Preferences`, `ExperienceItem`, `PersonalInfo`) directly correspond to the structure of the `hsresumebuilder.yaml` file.
    *   **Agent Note**: If you need to add a new field to the resume structure (like the `highlight` field was added to `ExperienceItem`), you must modify the data types here first. The `FromJSON` and `ToJSON` instances are derived generically, so Aeson handles YAML parsing and serialization based on these type definitions.

3.  **`src/ResumeBuilder/Themes/JoeTheme/Components.hs`**:
    *   **Purpose**: This file contains Haskell functions (using the Blaze HTML library) that render specific components of the resume into HTML. For example, `jExperienceItem` is responsible for rendering a single job experience entry.
    *   **Agent Note**: If you need to change *how* a part of the resume is displayed (e.g., change formatting, add new information to the HTML output for an existing data field), you will likely modify functions in this file.

4.  **`src/ResumeBuilder/Themes/JoeTheme/Template.hs`**:
    *   **Purpose**: This file defines the overall HTML structure and layout of the resume. It uses functions from `Components.hs` to build the complete HTML page.
    *   **Agent Note**: Changes to the main sections of the resume or the order in which they appear would typically be made here.

5.  **`src/ResumeBuilder/Lib.hs`**:
    *   **Purpose**: Contains the main application logic. It handles reading the configuration (which includes parsing `hsresumebuilder.yaml`), rendering the resume using the chosen theme and template, and saving the output to `output.html`.
    *   **Agent Note**: You typically won't need to modify this unless changing core application flow, like input file handling or output generation logic.

6.  **`app/Main.hs`**:
    *   **Purpose**: The main entry point of the executable. It calls functions from `ResumeBuilder.Lib`.
    *   **Agent Note**: Usually, no modifications are needed here for content or presentation changes.

7.  **`test/Spec.hs`**:
    *   **Purpose**: Contains test cases for the project.
    *   **Agent Note**: When adding new features or modifying existing logic, it's good practice to add or update tests here to verify correctness. The current test setup is basic; contributions to make it more robust are welcome.

## Common Tasks:

*   **Adding a new field to an experience item**:
    1.  Modify `ExperienceItem` in `src/ResumeBuilder/ResumeBuilderModel.hs`.
    2.  Update `jExperienceItem` in `src/ResumeBuilder/Themes/JoeTheme/Components.hs` to display the new field.
    3.  Add the new field to relevant entries in `hsresumebuilder.yaml`.
    4.  Consider adding a test to `test/Spec.hs`.

*   **Changing resume content (e.g., job description)**:
    1.  Directly edit the relevant section in `hsresumebuilder.yaml`.

*   **Changing HTML styling or structure for a specific component**:
    1.  Modify the relevant rendering function in `src/ResumeBuilder/Themes/JoeTheme/Components.hs`.

## Building and Running:

*   To build: `cabal build`
*   To run tests: `cabal test`
*   To generate the resume: `cabal run hsresumebuilder -- hsresumebuilder.yaml` (This will produce `output.html`)

**Agent Self-Correction Note**: Previously, I (Jules) mistakenly focused on `sample_resume.yaml` (a test file I created) instead of the main `hsresumebuilder.yaml` when debugging a YAML formatting issue. Always confirm which YAML file is the source of truth for resume data – it's `hsresumebuilder.yaml` in the project root.
