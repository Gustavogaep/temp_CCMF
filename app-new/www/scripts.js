// --- Hide top header and project page link when in an iframe ----------------

if (window.location !== window.parent.location) {
  // The page is in an iframe
  document.getElementById("back-to-project").style.display = "none";
  document.getElementById("top-header").style.display = "none";
}

// --- Jump to top button ------------------------------------------------------

//Get the button
var mybutton = document.getElementById("jump-top");

// When the user scrolls down 20px from the top of the document, show the button
window.onscroll = function () {
  scrollFunction();
};

function scrollFunction() {
  if (document.body.scrollTop > 20 || document.documentElement.scrollTop > 20) {
    mybutton.style.display = "block";
  } else {
    mybutton.style.display = "none";
  }
}

// When the user clicks on the button, scroll to the top of the document
function topFunction() {
  document.body.scrollTop = 0;
  document.documentElement.scrollTop = 0;
}

// Helper function to create popover help button
function helpButton(content, title = null) {
  let btn = document.createElement("a");
  btn.setAttribute("tabindex", "0");
  btn.setAttribute("class", "help-button");
  btn.setAttribute("role", "button");
  btn.setAttribute("data-toggle", "popover");
  btn.setAttribute("data-trigger", "focus");
  if (title != null) {
    btn.setAttribute("title", title);
  }
  btn.setAttribute("data-content", content);
  btn.innerHTML = '<i class="bi bi-question-circle"></i>';

  return btn;
}

// --- JQuery -----------------------------------------------------------------

$(document).ready(function () {
  // Toggle Show more / Show less button
  $(".btn-more-less").click(function () {
    var $this = $(this);
    if ($this.hasClass("collapsed")) {
      $this.text("Show less");
    } else {
      $this.text("Show more");
    }
  });

  // Insert popover help button for Incident Category user input widget
  $("#summary_incident_category")
    .parent()
    .before(
      helpButton(
        'An incident is designated as "Criminal" if the police are involved in any way, regardless of what stage the investigation/trial is at.'
      )
    );

  // Enable popovers
  $('[data-toggle="popover"]').popover();
});
