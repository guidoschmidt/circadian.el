hljs.highlightAll();

function switchHighlightThemes() {
  const isDarkMode = window.matchMedia?.(
    "(prefers-color-scheme: dark)",
  ).matches;
  const highlightStyleDark = document.querySelector(".theme.dark");
  const highlightStyleLight = document.querySelector(".theme.light");

  if (isDarkMode) {
    console.log("Dark Mode");
    highlightStyleLight.setAttribute("disabled", "disabled");
    highlightStyleDark.removeAttribute("disabled");
  } else {
    console.log("Light Mode");
    highlightStyleDark.setAttribute("disabled", "disabled");
    highlightStyleLight.removeAttribute("disabled");
  }
}

// Apply theme on page load
switchHighlightThemes();

// Listen for theme changes
if (window.matchMedia) {
  const themeMediaQuery = window.matchMedia("(prefers-color-scheme: dark)");
  themeMediaQuery.addListener(switchHighlightThemes);
}
