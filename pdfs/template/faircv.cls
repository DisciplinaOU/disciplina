\DeclareOption{russian}{\newcommand{\Language}{ru}\newcommand{\LongLanguage}{russian}}
\DeclareOption{english}{\newcommand{\Language}{en}\newcommand{\LongLanguage}{english}}
\ProcessOptions

\LoadClassWithOptions{extarticle}

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{faircv}[Disciplina Fair CV class]


\RequirePackage{titlesec}
\RequirePackage{longtable}
\RequirePackage{tabularx}
\RequirePackage{array}
\RequirePackage{url}
\RequirePackage{graphicx}
\RequirePackage{hyperref}
\RequirePackage{xltxtra}
\RequirePackage[\LongLanguage, useregional=numeric]{datetime2}

\RequirePackage[
    a4paper,
    left=15mm,
    right=15mm,
    top=7mm,
    bottom=15mm
]{geometry}

\RequirePackage[pages=some]{background}

\backgroundsetup{
    scale=1,
    color=black,
    angle=0,
    placement=top,
    firstpage=true,
    contents={%
        \includegraphics[width=\paperwidth]{images/header-background}
    }%
}

\input{labels/\Language}


% Real beauty
\setmainfont{ProximaNova-Regular.otf}[
    Path = fonts/ProximaNova/,
    ItalicFont = ProximaNova-RegularItalic.otf,
    BoldFont = ProximaNova-Bold.otf,
    BoldItalicFont = ProximaNova-BoldItalic.otf
]

\titleformat{\section}         % Customise the \section command
  {\Huge\bf\centering} % Make the \section headers large (\Large),
                               % small capitals (\scshape) and left aligned (\raggedright)
  {}{0em}                      % Can be used to give a prefix to all sections, like 'Section ...'
  {}                           % Can be used to insert code before the heading
\pagenumbering{gobble}
\urlstyle{same}

% Utility functions
\newcommand{\lw}[1]{\dimexpr #1\linewidth-2\tabcolsep}
\newcolumntype{Y}{>{\centering\arraybackslash}X}

% Template variables
\newcommand{\Name}[1]{\gdef\PrintName{#1}}%
\newcommand{\DateOfBirth}[3]{\gdef\PrintDateOfBirth{\DTMdate{#3-#2-#1}}} % Usage: \DateOfBirth{Day}{Month}{Year}
\newcommand{\QR}[1]{\gdef\PrintQR{#1}}%

% Diploma variables
\newcommand{\EducatorUrl}[1]{\def\PrintEducatorUrl{\url{#1}}}
\newcommand{\DateOfIssue}[3]{\def\PrintDateOfIssue{\DTMdate{#3-#2-#1}}} % Usage: \DateOfIssue{Day}{Month}{Year}
\newcommand{\DiplomaId}[1]{\def\PrintDiplomaId{#1}}
\newcommand{\DegreeLevel}[1]{\def\PrintDegreeLevel{#1}}
\newcommand{\Major}[1]{\def\PrintMajor{#1}}
\newcommand{\Minor}[1]{\def\PrintMinor{#1}}
\newcommand{\PartTimeEducation}{\def\IsPartTime{true}}

\newcommand{\EducationPeriod}[2]{%
    \def\EducationPeriodDefined{1}%
    \def\PrintEducationStartYear{#1}%
    \def\PrintEducationEndYear{#2}%
}

% Pictures and icons
\newcommand{\IconSize}{31px}
\newcommand{\Icon}[1]{\raisebox{-.25\height}{\includegraphics{images/#1.png}}~~}

\newcommand{\DiplomaIdIcon}{\Icon{diploma}}
\newcommand{\DegreeLevelIcon}{\Icon{degree-level}}
\newcommand{\MajorIcon}{\Icon{major}}
\newcommand{\MinorIcon}{\Icon{minor}}

% Block templates
\newenvironment{Header}{%
    \setlength{\parindent}{0pt}%
    \setlength{\parskip}{20pt}%
}{}

\newcommand{\MakeHeader}{%
    \begin{Header}
        \begin{minipage}[h]{0.68\linewidth}
            \raggedright
            \begin{Huge}
                \textbf{\PrintName}
            \end{Huge}

            \vspace*{10pt}
            \begin{Large}
                \tr{Date of birth:}\textbf{\PrintDateOfBirth}
            \end{Large}
        \end{minipage}
        \hfill
        \begin{minipage}[h]{0.28\linewidth}
            \raggedleft{\includegraphics[width=4cm]{\PrintQR}}
        \end{minipage}
    \end{Header}
    \vspace*{40pt}
}


\newenvironment{Diploma}{%
    \setlength{\parindent}{0pt}%
    \large
}{%
    \ifdefined\PrintEducatorUrl
        \begin{center}
            \Icon{url}\PrintEducatorUrl
        \end{center}
    \fi

    \setlength{\parskip}{12pt}%

    \vspace*{15pt}

    \ifdefined\EducationPeriodDefined%
        \begin{tabularx}{\textwidth}{Y | Y | Y}
            \hline
            \multicolumn{3}{c}{~} \\
            \tr{Year of admission} \textbf{\PrintEducationStartYear} & %
            \tr{Year of graduation} \textbf{\PrintEducationEndYear} & %
            \ifdefined\IsPartTime%
                \tr{Part-time education}%
            \else%
                \tr{Full-time education}%
            \fi%
            \\
            \multicolumn{3}{c}{~} \\
            \hline
        \end{tabularx}
    \fi

    \vspace*{30pt}

    \setlength\extrarowheight{12pt}%
    \begin{tabularx}{\textwidth}{%
            @{} c @{}%
            l%
            >{\raggedright\arraybackslash}X%
        }
        \\[-3\normalbaselineskip]
        \ifdefined\PrintDiplomaId
            \Icon{diploma} & \tr{Diploma} & \textbf{\PrintDiplomaId} %
            \ifdefined\PrintDateOfIssue%
                \textbf{(\PrintDateOfIssue)}%
            \fi%
            \\
        \fi

        \ifdefined\PrintDegreeLevel
            \Icon{degree-level} & \tr{Degree} & \textbf{\PrintDegreeLevel} \\
        \fi

        \ifdefined\PrintMajor
            \Icon{major} & \tr{Major} & \textbf{\PrintMajor} \\
        \fi

        \ifdefined\PrintMinor
            \Icon{minor} & \tr{Minor} & \textbf{\PrintMinor} \\
        \fi
    \end{tabularx}

    \vspace*{30pt}
}

\newenvironment{Courses}{%
    \setlength{\parindent}{0pt}%
    \setlength\extrarowheight{12pt}%
    \longtable{%
            >{\raggedright\arraybackslash}p{\lw{0.52}}%
            p{\lw{0.12}}%
            p{\lw{0.12}}%
            p{\lw{0.12}}%
            >{\raggedleft\arraybackslash}p{\lw{0.12}}%
        }
        \textbf{\tr{Course}} %
        & \textbf{\tr{Language}} %
        & \textbf{\tr{Hours}} %
        & \textbf{\tr{ECTS credits}} %
        & \textbf{\tr{Grade}} %
        \\[1.5\normalbaselineskip] \hline \endhead%
}{%
    \endlongtable
}
