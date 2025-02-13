# Delphi Bücherei Bibliothek

Dieses Projekt ist eine modulare Bücherei-Anwendung, die in Delphi unter Verwendung der VCL (Visual Component Library) entwickelt wurde. Ziel des Projekts ist es, eine robuste, wartbare und erweiterbare Softwarelösung für die Verwaltung einer Bücherei zu erstellen.

## Inhaltsverzeichnis

- [Projektübersicht](#projektübersicht)
- [Features](#features)
- [Module und UI-Struktur](#module-und-ui-struktur)
- [Einstellungen](#einstellungen)
- [Vorgehensweise](#vorgehensweise)
- [Technologien](#technologien)
- [Installation & Konfiguration](#installation--konfiguration)
- [Beitragende](#beitragende)
- [Lizenz](#lizenz)

## Projektübersicht

Die Anwendung ermöglicht es Benutzern, die folgenden Funktionen zu nutzen:
- **Kundenverwaltung:** Anlegen, Bearbeiten, Löschen und Suchen von Kunden.
- **Buchverwaltung:** Erfassung und Verwaltung von Büchern inklusive physischer Zuordnung (Regal, Platznummer).
- **Buchsuche:** Erweiterte Suche und Filterung von Büchern.
- **Ausleihvorgang:** Erfassung, Rückgabe und Verlängerung von Ausleihen.
- **Mahnungsverwaltung:** Versenden von Mahnungen per E-Mail und Überwachung des Mahnungsstatus.
- **Einstellungen:** Konfiguration der Datenbank, E-Mail-Versand (SMTP/IMAP) und zukünftig Mehrsprachigkeit.

## Features

- **Modulare Architektur:**  
  Die Anwendung verwendet VCL-Frames für eine klare Trennung der einzelnen Module (Kunden, Bücher, Ausleihe, Mahnungen, Einstellungen).

- **Datenbank- und E-Mail-Konfiguration:**  
  Alle systemrelevanten Einstellungen können zentral in einem eigenen Modul verwaltet werden.

- **Erweiterbarkeit:**  
  Die Software ist so aufgebaut, dass zukünftige Funktionen wie Mehrsprachigkeit oder Reporting-Tools einfach integriert werden können.

## Module und UI-Struktur

Die Anwendung besteht aus folgenden Modulen:

| **Modul / Formular**  | **Funktion**                                                                 | **Basisfelder / Features**                                                                                                                                                              |
|-----------------------|------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Kundenverwaltung**  | Anlegen, Bearbeiten, Löschen und Suchen von Kunden                           | Name, Vorname, Geburtsdatum, Geburtsort, PLZ, Ort, Straße, Hausnummer, E-Mail, Telefonnummer, Kundennummer; Validierung, Status, Notizen                                            |
| **Buchverwaltung**    | Erfassung und Verwaltung von Büchern                                         | Name, Genre, ISBN, Autor, Herausgeber, Auflage; physische Zuordnung (Regal, Platznummer), zusätzliche Felder wie Sprache, Seitenanzahl, Publikationsjahr, Barcode, Status                |
| **Buchsuche**         | Dynamische Suche und Filterung von Büchern                                   | Suchfelder für Name, Genre, ISBN, Autor, Herausgeber, Auflage; erweiterte Filteroptionen (z. B. Regal, Platznummer)                                                                     |
| **Ausleihvorgang**    | Ausleihen, Rückgabe und Verlängerung von Büchern                             | Kundennummer, Ausleihtag, Ausleihfrist, Verlängerungsoptionen; automatische Berechnung des Soll-Rückgabedatums, Statusanzeige (aktuell, überfällig)                                  |
| **Mahnungsverwaltung**| Erfassen und Versenden von Mahnungen per E-Mail                              | Kundendaten, ausgeliehene Bücher, Fristüberfälligkeit, Bemerkungen; Mahnungsnummer, Mahndatum, eventuelle Strafgebühren, Historie der versendeten Mahnungen                          |
| **Einstellungen**     | Konfiguration von Datenbank- und E-Mail-Versandoptionen, zukünftige Mehrsprachigkeit | Datenbank: Serveradresse, Port, Datenbankname, Benutzername, Passwort; E-Mail: SMTP/IMAP-Server, Port, Benutzername, Passwort, Verschlüsselung; Mehrsprachigkeit: Dropdown zur Sprachauswahl |

## Einstellungen

Das Einstellungen-Modul ermöglicht es, alle systemrelevanten Parameter zentral zu konfigurieren:

- **Datenbank-Konfiguration:**  
  Serveradresse, Port, Datenbankname, Benutzername, Passwort und Testfunktion zur Verbindungsüberprüfung.

- **E-Mail-Konfiguration:**  
  SMTP-/IMAP-Servereinstellungen, Port, Benutzername, Passwort, Verschlüsselungsoptionen (SSL/TLS) sowie eine Testfunktion für den E-Mail-Versand.

- **Mehrsprachigkeit (zukünftig):**  
  Möglichkeit zur Auswahl der Anwendungssprache (z. B. Deutsch, Englisch), um das Projekt später mehrsprachig zu gestalten.

## Vorgehensweise

1. **Modulare UI-Entwicklung mit VCL-Frames:**  
   Jedes Modul (Kundenverwaltung, Buchverwaltung, Ausleihe, Mahnungen, Einstellungen) wird als eigenständiger VCL-Frame realisiert, der in das Hauptformular eingebettet wird.

2. **Einheitliches Haupt-Dashboard:**  
   Ein zentrales Hauptfenster mit Navigation (Menüleiste, Toolbar, Statusanzeige) ermöglicht den schnellen Zugriff auf alle Module.

3. **Trennung von Logik und Darstellung:**  
   Geschäftslogik und Datenzugriff werden von der UI getrennt implementiert, um eine wartbare und erweiterbare Architektur zu gewährleisten.

## Technologien

- **Delphi (VCL):**  
  Verwendung der VCL für die Entwicklung der Desktop-Anwendung.

- **Datenbankzugriff:**  
  Einsatz von FireDAC oder einer vergleichbaren Komponente zur Anbindung der Datenbank.

- **E-Mail-Versand:**  
  Implementierung von SMTP/IMAP für den E-Mail-Versand und -Empfang.

## Installation & Konfiguration

1. **Repository klonen:**

   ```bash
   git clone https://github.com/dein-benutzername/delphi-buecherei.git
   cd delphi-buecherei
