// Here, you can define all custom functions, you want to use and initialize some variables

const coin = _.sample(["head", "tail"]); // You can determine global (random) parameters here
// Declare your variables here

/* For generating random participant IDs */
// https://stackoverflow.com/questions/1349404/generate-random-string-characters-in-javascript
// dec2hex :: Integer -> String
const dec2hex = function(dec) {
  return ("0" + dec.toString(16)).substr(-2);
};
// generateId :: Integer -> String
const generateID = function(len) {
  let arr = new Uint8Array((len || 40) / 2);
  window.crypto.getRandomValues(arr);
  return Array.from(arr, this.dec2hex).join("");
};

// Error feedback if participants exceeds the time for responding
const time_limit = function(data, next) {
  if (typeof window.timeout === "undefined") {
    window.timeout = [];
  }
  // Add timeouts to the timeoutarray
  // Reminds the participant to respond after 5 seconds
  window.timeout.push(
    setTimeout(function() {
      $("#reminder").text("Please answer more quickly!");
    }, 5000)
  );
  next();
};

// compares the chosen answer to the value of `option1`
check_response = function(data, next) {
  $("input[name=answer]").on("change", function(e) {
    if (e.target.value === data.correct) {
      alert("Your answer is correct! Yey!");
    } else {
      alert(
        "Sorry, this answer is incorrect :( The correct answer was " +
          data.correct
      );
    }
    next();
  });
};

// custom functions:

// function to randomly order the four utterences, given per trial
function random_utternace(slider_rating_trials) {
  for (var i = 0; i < slider_rating_trials.length; i++) {
    slider_rating_trials[i].allUtterances = _.shuffle([
      slider_rating_trials[i].question1,
      slider_rating_trials[i].question2,
      slider_rating_trials[i].question3,
      slider_rating_trials[i].question4
    ]);
  }
  return slider_rating_trials;
}
