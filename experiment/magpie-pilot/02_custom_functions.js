// Here, you can define all custom functions, you want to use and initialize some variables

const group = _.sample(["group1", "group2"]); // You can determine global (random) parameters here

// Declare your variables here

/* For generating random participant IDs */
// https://stackoverflow.com/questions/1349404/generate-random-string-characters-in-javascript
// dec2hex :: Integer -> String
const dec2hex = function (dec) {
  return ("0" + dec.toString(16))
    .substr(-2);
};
// generateId :: Integer -> String
const generateID = function (len) {
  let arr = new Uint8Array((len || 40) / 2);
  window.crypto.getRandomValues(arr);
  return Array.from(arr, this.dec2hex)
    .join("");
};

// Error feedback if participants exceeds the time for responding
const time_limit = function (data, next) {
  if (typeof window.timeout === "undefined") {
    window.timeout = [];
  }
  // Add timeouts to the timeoutarray
  // Reminds the participant to respond after 5 seconds
  window.timeout.push(
    setTimeout(function () {
      $("#reminder")
        .text("Please answer more quickly!");
    }, 5000)
  );
  next();
};

// compares the chosen answer to the value of `option1`
check_response = function (data, next) {
  $("input[name=answer]")
    .on("change", function (e) {
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
function random_utterance(slider_rating_trials=[{}, {}, {}, {}]) {
  for (var i = 0; i < slider_rating_trials.length; i++) {
    // slider_rating_trials[i].question1 = "The green block and the blue block will touch the ground.";
    // slider_rating_trials[i].question2 = "The green block will touch the ground, but the blue block will not touch the ground.";
    // slider_rating_trials[i].question3 = "The blue block will touch the ground, but the green block will not touch the ground.";
    // slider_rating_trials[i].question4 = "Neither the green block nor the blue block will touch the ground.";
    let utterances = ["<b>Blue will</b> and <b>green will</b> touch the ground.",
                      "<b>Blue will not</b> and <b>green will</b> touch the ground.",
                      "<b>Blue will</b> and <b>green will not</b> touch the ground.",
                      "<b>Blue will not</b> and <b>green will not</b> touch the ground."
                    ];
    slider_rating_trials[i].allUtterances = _.shuffle(utterances);
    slider_rating_trials[i].question1 = slider_rating_trials[i].allUtterances[0];
    slider_rating_trials[i].question2 = slider_rating_trials[i].allUtterances[1];
    slider_rating_trials[i].question3 = slider_rating_trials[i].allUtterances[2];
    slider_rating_trials[i].question4 = slider_rating_trials[i].allUtterances[3];
  }
  return slider_rating_trials;
}

repliedAll = function(r1, r2, r3, r4, counter=0){
  return (r1.hasClass('replied') &&
          r2.hasClass('replied') &&
          r3.hasClass('replied') &&
          r4.hasClass('replied')|| counter>=3);
}
toggleNextIfDone = function (button, condition) {
    if(condition){
      button.removeClass("grid-button");
    }
};
