// In this file you can create your own custom view templates

// A view template is a function that returns a view,
// this functions gets some config (e.g. trial_data, name, etc.) information as input
// A view is an object, that has a name, CT (the counter of how many times this view occurred in the experiment),
// trials the maximum number of times this view is repeated
// and a render function, the render function gets CT and the magpie-object as input
// and has to call magpie.findNextView() eventually to proceed to the next view (or the next trial in this view),
// if it is an trial view it also makes sense to call magpie.trial_data.push(trial_data) to save the trial information

// generate a new multi_slider

const multi_slider_generator = {
  // we do not want to show the picture in the stimulus container anymore, but in the grid
  // together with the answer_container
  stimulus_container_gen: function(config, CT) {
    return `<div class='magpie-view'>
                         <h1 class='magpie-view-title'>${config.title}</h1>
                     </div>`;
  },

  answer_container_gen: function(config, CT) {
    const option1 = config.data[CT].optionLeft;
    const option2 = config.data[CT].optionRight;
    return `<div class='magpie-multi-slider-grid' id='target'>
              <picture class='grid-picture'>
                <img class='image-stretch' src=${config.data[CT].picture}>
              </picture>
              <header class='magpie-grid-qud magpie-view-question magpie-view-qud'>
                <strong>${config.data[CT].QUD}</strong>
              </header>
              <question1 class='magpie-view-question' id ='question1' >${config.data[CT].allUtterances[0]}</question1>
              <slider1 class='magpie-grid-slider'>
                <span class='magpie-response-slider-option'>${option1}</span>
                <input type='range' id='response1' name='answer1' class='magpie-response-slider' min='0' max='100' value='50'/>
                <span class='magpie-response-slider-option'>${option2}</span>
              </slider1>
              <question2 class='magpie-view-question' id ='question2' >${config.data[CT].allUtterances[1]}</question2>
              <slider2 class='magpie-grid-slider'>
                <span class='magpie-response-slider-option'>${option1}</span>
                <input type='range' id='response2' name='answer2' class='magpie-response-slider' min='0' max='100' value='50'/>
                <span class='magpie-response-slider-option'>${option2}</span>
              </slider2>
              <question3 class='magpie-view-question' id ='question3' >${config.data[CT].allUtterances[2]}</question3>
              <slider3 class='magpie-grid-slider'>
                <span class='magpie-response-slider-option'>${option1}</span>
                <input type='range' id='response3' name='answer3' class='magpie-response-slider' min='0' max='100' value='50'/>
                <span class='magpie-response-slider-option'>${option2}</span>
              </slider3>
              <question4 class='magpie-view-question' id ='question3' >${config.data[CT].allUtterances[3]}</question4>
              <slider4 class='magpie-grid-slider'>
                <span class='magpie-response-slider-option'>${option1}</span>
                <input type='range' id='response4' name='answer4' class='magpie-response-slider' min='0' max='100' value='50'/>
                <span class='magpie-response-slider-option'>${option2}</span>
              </slider4>
              </div>
              <button id='buttonNext' class='grid-button magpie-view-button'>Next scene</button>`;
  },

  handle_response_function: function(
    config,
    CT,
    magpie,
    answer_container_generator,
    startingTime
  ) {
    let response1;
    let response2;
    let response3;
    let response4;

    $(".magpie-view").append(answer_container_generator(config, CT));

    response1 = $("#response1");
    response2 = $("#response2");
    response3 = $("#response3");
    response4 = $("#response4");

    // function for debugging - if "y" is pressed, the slider will change
    // the next button has to be pressed, in order to get to next trial
    var counter = 0;
    let isAnswered = {"q1": false, "q2": false, "q3": false, "q4": false}
    document.addEventListener("keydown", event => {
      var keyName = event.key;

      if (keyName === "y") {
        if (counter == 0) {
          var s = document.getElementById("response1");
          s.value = Math.floor(Math.random() * 101);
          $("#utterance1").toggleClass("magpie-nodisplay");
          $("#utterance2").removeClass("magpie-nodisplay");
          isAnswered.q1 = true;
          console.log(s.value);
          counter += 1;
          console.log(counter);
        } else if (counter == 1) {
          var t = document.getElementById("response2");
          t.value = Math.floor(Math.random() * 101);
          $("#utterance2").toggleClass("magpie-nodisplay");
          $("#utterance3").removeClass("magpie-nodisplay");
          isAnswered.q2 = true;
          console.log(t.value);
          counter += 1;
          console.log(counter);
        } else if (counter == 2) {
          var u = document.getElementById("response3");
          u.value = Math.floor(Math.random() * 101);
          //$("#button4").removeClass("magpie-nodisplay");
          $("#utterance3").toggleClass("magpie-nodisplay");
          $("#utterance4").removeClass("magpie-nodisplay");
          isAnswered.q3 = true;
          console.log(u.value);
          counter += 1;
          console.log(counter);
        } else if (counter == 3) {
          var v = document.getElementById("response4");
          v.value = Math.floor(Math.random() * 101);
          $("#utterance4").toggleClass("magpie-nodisplay");
          isAnswered.q4 = true; toggleNextIfDone();
          console.log(v.value);
          counter += 1;
          console.log(counter);
        } else if (counter == 4) {
          $("#runButton").removeClass("magpie-nodisplay");
          $("#next").removeClass("magpie-nodisplay");
          console.log(counter);
        }
      }
      return keyName;
    });

    // check the sliders for all 4 utterance and handle next button
    // this is code without debut mode

    toggleNextIfDone = function(){
      if (isAnswered.q1 && isAnswered.q2 && isAnswered.q3 && isAnswered.q4) {
        $("#buttonNext").removeClass("grid-button")
      };
    }
    response1.on("change", function() {
      isAnswered.q1 = true;
      toggleNextIfDone();
    });

    response2.on("change", function() {
      isAnswered.q2 = true;
      toggleNextIfDone();
    });

    response3.on("change", function() {
      isAnswered.q3 = true;
      toggleNextIfDone();
    });

    response4.on("change", function() {
      isAnswered.q4 = true;
      toggleNextIfDone();
    });

    $("#buttonNext").on("click", function() {
      const RT = Date.now() - startingTime; // measure RT before anything else
      let trial_data = {
        trial_name: config.name,
        trial_number: CT + 1,
        response: [
          $("#response1").val(),
          $("#response2").val(),
          $("#response3").val(),
          $("#response4").val()
        ],
        RT: RT
      };

      trial_data = magpieUtils.view.save_config_trial_data(
        config.data[CT],
        trial_data
      );
      magpie.trial_data.push(trial_data);
      magpie.findNextView();
    });
  }
};
