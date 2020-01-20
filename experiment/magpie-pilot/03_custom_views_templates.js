// In this file you can create your own custom view templates

// A view template is a function that returns a view,
// this functions gets some config (e.g. trial_data, name, etc.) information as input
// A view is an object, that has a name, CT (the counter of how many times this view occurred in the experiment),
// trials the maximum number of times this view is repeated
// and a render function, the render function gets CT and the magpie-object as input
// and has to call magpie.findNextView() eventually to proceed to the next view (or the next trial in this view),
// if it is an trial view it also makes sense to call magpie.trial_data.push(trial_data) to save the trial information

const animation_view  = {
    name: "animation",
    title: "title",
    CT: 0,
    trials: allScenes.length,
    data: "",
    // The render function gets the magpie object as well as the current trial in view counter as input
    render: function(CT, magpie){
      let startTime = Date.now();
      const view_template = `
        <div class='magpie-view-stimulus-grid'>
          <animationTitle class='stimulus'>
            <h1>Click on Run to see what will happen!</h1>
          </animationTitle>
          <animation id='animationDiv'>
            <canvas id='animationCanvas'></canvas>
          </animation>
          <run>
            <button id="runButton" class="magpie-view-button">Run</button>
          </run>
          <next>
            <button id='buttonNextAnimation' class='magpie-view-button grid-button'>Next scenario</button>
          </next>
        </div>
      `;
      $('#main').html(view_template);

      console.log(allScenes);
      let sceneData = defineScene(allScenes[CT]);
      let worldObjects = createScene(allScenes[CT]["platform.type"], sceneData, "train");
      showScene(worldObjects, document.getElementById('animationDiv'));

      let nbClicks = 0;
      $('#runButton').on('click', function(e){
        nbClicks += 1;
        if(nbClicks === 1) {
          runAnimation(worldObjects);
        }
      });
      $("#buttonNextAnimation").on("click", function () {
          magpie.findNextView();
      });
    }
};

// generate a new multi_slider
const multi_slider_generator = {
  stimulus_container_gen: function (config, CT) {
    return `<div class='magpie-view'>
      <h1 class='stimulus'>
      ${config.data[CT].QUD}
      </h1>
      <div class='stimulus'>
      <img src=${config.data[CT].picture}>
      </div>
      </div>`;
  },

  answer_container_gen: function (config, CT) {
    const option1 = config.data[CT].optionLeft;
    const option2 = config.data[CT].optionRight;
    return `<div class='magpie-multi-slider-grid' id='target'>
              <question1 class='magpie-view-question grid-question' id ='question1' >${
                config.data[CT].question1
              }</question1>
              <slider1 class='magpie-grid-slider' id='slider1'>
                <span class='magpie-response-slider-option optionWide'>${option1}</span>
                <input type='range' id='response1' name='answer1' class='magpie-response-slider' min='0' max='100' value='50' oninput='output1.value = response1.value + "%"'/>
                <span class='magpie-response-slider-option optionWide'>${option2}</span>
                <output name="outputSlider1" id="output1" class="thick">50%</output>
              </slider1>
              <question2 class='magpie-view-question grid-question' id ='question2' >${
                config.data[CT].question2
              }</question2>
              <slider2 class='magpie-grid-slider' id='slider2'>
                <span class='magpie-response-slider-option optionWide'>${option1}</span>
                <input type='range' id='response2' name='answer2' class='magpie-response-slider' min='0' max='100' value='50' oninput='output2.value = response2.value + "%"'/>
                <span class='magpie-response-slider-option optionWide'>${option2}</span>
                <output name="outputSlider2" id="output2" class="thick">50%</output>
              </slider2>
              <question3 class='magpie-view-question grid-question' id ='question3' >${
                config.data[CT].question3
              }</question3>
              <slider3 class='magpie-grid-slider' id='slider3'>
                <span class='magpie-response-slider-option optionWide'>${option1}</span>
                <input type='range' id='response3' name='answer3' class='magpie-response-slider' min='0' max='100' value='50' oninput='output3.value = response3.value + "%"'/>
                <span class='magpie-response-slider-option optionWide'>${option2}</span>
                <output name="outputSlider3" id="output3" class="thick">50%</output>
              </slider3>
              <question4 class='magpie-view-question grid-question' id ='question4' >${
                config.data[CT].question4
              }</question4>
              <slider4 class='magpie-grid-slider' id='slider4'>
                <span class='magpie-response-slider-option optionWide'>${option1}</span>
                <input type='range' id='response4' name='answer4' class='magpie-response-slider' min='0' max='100' value='50' oninput='output4.value = response4.value + "%"'/>
                <span class='magpie-response-slider-option optionWide'>${option2}</span>
                <output name="outputSlider4" id="output4" class="thick">50%</output>
              </slider4>
              </div>
              <button id='buttonNext' class='grid-button magpie-view-button'>Next scenario</button>`;
  },

  handle_response_function: function (
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
    document.addEventListener("keydown", event => {
      var keyName = event.key;

      if (keyName === "y") {
        if (counter == 0) {
          var s = document.getElementById("response1");
          s.value = Math.floor(Math.random() * 101);
          $('#response1').addClass('replied')
          counter += 1;
        } else if (counter == 1) {
          var t = document.getElementById("response2");
          t.value = Math.floor(Math.random() * 101);
          $('#response2').addClass('replied')
          counter += 1;
        } else if (counter == 2) {
          var u = document.getElementById("response3");
          u.value = Math.floor(Math.random() * 101);
          $('#response3').addClass('replied')
          counter += 1;
        } else if (counter == 3) {
          var v = document.getElementById("response4");
          v.value = Math.floor(Math.random() * 101);
          $('#response4').addClass('replied')
          toggleNextIfDone();
          counter += 1;
        }
      }
      return keyName;
    });
    // check the sliders for all 4 utterance and handle next button
    // this is code without debut mode
    repliedAll = function(){
      return (response1.hasClass('replied') &&
              response2.hasClass('replied') &&
              response3.hasClass('replied') &&
              response4.hasClass('replied')|| counter>=3);
    }
    toggleNextIfDone = function () {
        if(repliedAll()){
          $("#buttonNext").removeClass("grid-button");
        }
    };
    response1.on("change", function () {
      $('#response1').addClass('replied');
      toggleNextIfDone();
    });

    response2.on("change", function () {
      $('#response2').addClass('replied')
      toggleNextIfDone();
    });

    response3.on("change", function () {
      $('#response3').addClass('replied')
      toggleNextIfDone();
    });

    response4.on("change", function () {
      $('#response4').addClass('replied')
      toggleNextIfDone();
    });

    $("#buttonNext").on("click", function () {
        const RT = Date.now() - startingTime; // measure RT before anything else
        let trial_data = {
          trial_name: config.name,
          trial_number: CT + 1,
          response: [$("#response1").val(), $("#response2").val(),
                      $("#response3").val(), $("#response4").val()
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
