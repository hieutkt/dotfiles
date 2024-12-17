\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{hpcheatsheet}[2024/12/10 A simpler cheatsheet class with title & author in header, LuaLaTeX, no extra configs but calc loaded]

% Load the base class
\LoadClassWithOptions{article}

% Handle class options
\RequirePackage{xkeyval}

% Default colors and columns
\newcommand*\CheatSheetBgColor{white}
\newcommand*\CheatSheetFgColor{black}
\newcommand*\CheatSheetColumns{4}
\newcommand*\CheatSheetFontSize{ssmall}

\DeclareOptionX{bgcolor}{\renewcommand*\CheatSheetBgColor{#1}}
\DeclareOptionX{fgcolor}{\renewcommand*\CheatSheetFgColor{#1}}
\DeclareOptionX{columns}{\renewcommand*\CheatSheetColumns{#1}}
\DeclareOptionX{fontsize}{\renewcommand*\CheatSheetFontSize{#1}}

\ProcessOptionsX\relax

% Detect engine
\RequirePackage{iftex}

\ifLuaTeX
  % LuaLaTeX settings
  \RequirePackage{fontspec}
  \defaultfontfeatures{Ligatures=TeX}
  \setmainfont{TeX Gyre Pagella}
\else
  % pdfLaTeX settings
  \RequirePackage[T1]{fontenc}
  \RequirePackage{lmodern}
\fi

% Essential packages
\RequirePackage[framemethod=TikZ]{mdframed}
\RequirePackage{amsthm}
\RequirePackage[landscape]{geometry}
\RequirePackage{multicol}
\RequirePackage{tikz}
\usetikzlibrary{calc} % Needed for coordinate calculations
\RequirePackage{xcolor}
\RequirePackage{amsmath}
\RequirePackage{changepage}
\RequirePackage{amssymb}
\RequirePackage{fancyhdr}
\RequirePackage[many]{tcolorbox}
\RequirePackage{moresize}
\RequirePackage{fullpage}
\RequirePackage{cancel}

% Basic settings
% \setlength{\baselineskip}{1.2em}
% \setlength{\parskip}{-0.75em}

% Define some colors
\definecolor{r1}{RGB}{255, 191, 191}
\definecolor{r2}{RGB}{255, 191, 223}
\definecolor{r3}{RGB}{255, 207, 207}

\definecolor{b1}{RGB}{191, 223, 255}
\definecolor{b2}{RGB}{191, 239, 255}
\definecolor{b3}{RGB}{191, 255, 255}

\definecolor{g1}{RGB}{191, 255, 191}
\definecolor{g2}{RGB}{191, 255, 223}
\definecolor{g3}{RGB}{207, 255, 207}

\definecolor{o1}{RGB}{255, 223, 191}
\definecolor{o2}{RGB}{255, 239, 191}
\definecolor{o3}{RGB}{255, 231, 191}

\definecolor{v1}{RGB}{223, 191, 255}
\definecolor{v2}{RGB}{239, 191, 255}
\definecolor{v3}{RGB}{231, 191, 255}

\definecolor{y1}{RGB}{255, 255, 191}
\definecolor{y2}{RGB}{255, 247, 191}
\definecolor{y3}{RGB}{255, 239, 191}

\definecolor{w}{HTML}{eeeeee}
\definecolor{g}{HTML}{444444}
\definecolor{b}{HTML}{222222}
\definecolor{lightgrey}{HTML}{cccccc}

% Geometry adjustments
\geometry{
    letterpaper,
    left=0.25in,
    right=0.25in,
    top=0.3in,
    bottom=0.25in
}

\parindent0pt

\newtcolorbox{conceptbox}[2][]{
  breakable,
  vfill before first=false,
  segmentation at break=false,
  size=fbox,
  title=\scriptsize\textbf{\MakeUppercase{#2}},
  left=2pt,
  right=2pt,
  top=3pt,
  bottom=1pt,
  boxrule=1pt,
  coltitle=\CheatSheetFgColor,
  colupper=\CheatSheetFgColor,
  pad at break=5pt,
  toprule at break=4pt,
  bottomrule at break=0.75pt,
  colframe=#1,
  colback=\CheatSheetBgColor!90!#1,
  enlargepage=12in,
  before skip = -2em,
  after skip = 2.5em
}

% Line node command (requires calc)
\newcommand{\nc}[2][b]{%
\newline \tikz \draw [draw=#1,thick]
    ($(current page.center)-(0.495\linewidth,0)$) --
    ($(current page.center)+(0.495\linewidth,0)$)
    node [midway, fill=\CheatSheetBgColor!90!#1, text=\CheatSheetFgColor] {\ssmall\textbf{\uppercase{#2}}};
}

% Booleans
\newif\ifboxopen
\boxopenfalse

\newif\ifcontentstarted
\contentstartedfalse

\newcommand{\startcolumn}{%
  \strut\par
}

% \boxtitle command
\newcommand{\boxtitle}[2][v3]{%
  \ifboxopen
    \end{conceptbox}
    \boxopenfalse
  \fi
  \begin{conceptbox}[#1]{#2}
  \boxopentrue
}

% \sectiontitle command
\newcommand{\sectiontitle}[2][v3]{%
  \nc[#1]{#2}\\[0.5em]
}

% \boxcolumnbreak command
\newcommand{\boxcolumnbreak}{%
  \ifcontentstarted
    \ifboxopen
      \end{conceptbox}
      \boxopenfalse
    \fi
    \columnbreak
    \startcolumn
  \else
    \PackageError{cheatsheet}{\string\boxcolumnbreak\space used before multicols started}{%
      This command can only be used after the document starts.}%
  \fi
}

% \boxpagebreak command
\newcommand{\boxpagebreak}{%
  \ifcontentstarted
    \ifboxopen
      \end{conceptbox}
      \boxopenfalse
    \fi
    \newpage
    \startcolumn
  \else
    \PackageError{cheatsheet}{\string\boxpagebreak\space used before multicols started}{%
      This command can only be used after the document starts.}%
  \fi
}


% Custom header style using TikZ (old style)
\renewcommand{\headrulewidth}{0pt}
\newcommand{\cheatsheetheader}{%
  \vspace{-1.5em}
  \begin{tikzpicture}
    \pgfmathsetlengthmacro{\FullWidth}{\textwidth}
    \pgfmathsetlengthmacro{\LineEnd}{\textwidth-0.25in}
    \draw[thick, color=\CheatSheetFgColor] (1,0) -- (\LineEnd,0);
    \node[anchor=west, fill=\CheatSheetBgColor!90!\CheatSheetFgColor, inner sep=3pt, text=\CheatSheetFgColor] at (0,0) {\ssmall\textbf{\MakeUppercase{\@title}}};
    \node[anchor=east, fill=\CheatSheetBgColor!90!\CheatSheetFgColor, inner sep=3pt, text=\CheatSheetFgColor] at (\FullWidth,0) {\ssmall\textbf{\MakeUppercase{\@author}}};
  \end{tikzpicture}%
}

\renewcommand\maketitle{}


\AtBeginDocument{%
  \pagecolor{\CheatSheetBgColor}%
  \color{\CheatSheetFgColor}%
  \csname\CheatSheetFontSize\endcsname%
  \pagestyle{fancy}
  \fancyhf{}
  \fancyheadoffset{0pt}
  \setlength{\headwidth}{\textwidth}
  \fancyhead[C]{\cheatsheetheader}
  \begin{multicols*}{\CheatSheetColumns}
    \raggedcolumns
    \startcolumn
    \contentstartedtrue
  }

\AtEndDocument{%
  \ifboxopen
    \end{conceptbox}
  \fi
  \ifcontentstarted
    \end{multicols*}
  \fi
}
\endinput
