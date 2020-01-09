// In this file you can instantiate your views
// We here first instantiate wrapping views, then the trial views

/** Wrapping views below

* Obligatory properties

    * trials: int - the number of trials this view will appear
    * name: string

*Optional properties
    * buttonText: string - the text on the button (default: 'next')
    * text: string - the text to be displayed in this view
    * title: string - the title of this view

    * More about the properties and functions of the wrapping views - https://magpie-ea.github.io/magpie-docs/01_designing_experiments/01_template_views/#wrapping-views

*/

// Every experiment should start with an intro view. Here you can welcome your participants and tell them what the experiment is about
const intro = magpieViews.view_generator("intro", {
  trials: 1,
  name: "intro",
  // If you use JavaScripts Template String `I am a Template String`, you can use HTML <></> and javascript ${} inside
  text: `Thank you for your participation in our study!
         Your anonymous data makes an important contribution to our understanding of human language use.
          <br />
          <br />
          Legal information:
          By answering the following questions, you are participating in a study
          being performed by scientists from the University of Osnabrueck.
          <br />
          <br />
          You must be at least 18 years old to participate.
          <br />
          <br />
          Your participation in this research is voluntary.
          You may decline to answer any or all of the following questions.
          You may decline further participation, at any time, without adverse consequences.
          <br />
          <br />
          Your anonymity is assured; the researchers who have requested your
          participation will not receive any personal information about you.
          `,
  buttonText: "begin the experiment"
});

// For most tasks, you need instructions views
const instructions = magpieViews.view_generator("instructions", {
  trials: 1,
  name: "instructions",
  title: "General Instructions",
  text: `In this experiment you are asked to what degree you agree or disagree
        with statements describing a situation.
        For each situation you are aked to judge four statements.
        There are 22 situations in total.
            <br />
            <br />
        The experiment will take you about ... minutes.
        You will be shown an example first.`,
  buttonText: "go to example trials"
});

const instructions2 = magpieViews.view_generator("instructions", {
  trials: 1,
  name: "instructions2",
  title: "Instructions",
  text: `We will now move on to the main part of the experiment.
        <br />
        <br />
        Keep in mind  ....  `,
  buttonText: "Start main experiment"
});

// In the post test questionnaire you can ask your participants addtional questions
const post_test = magpieViews.view_generator("post_test", {
  trials: 1,
  name: "post_test",
  title: "Additional information",
  text: "Answering the following questions is optional, but your answers will help us analyze our results."

  // You can change much of what appears here, e.g., to present it in a different language, as follows:
  // buttonText: 'Weiter',
  // age_question: 'Alter',
  // gender_question: 'Geschlecht',
  // gender_male: 'männlich',
  // gender_female: 'weiblich',
  // gender_other: 'divers',
  // edu_question: 'Höchster Bildungsabschluss',
  // edu_graduated_high_school: 'Abitur',
  // edu_graduated_college: 'Hochschulabschluss',
  // edu_higher_degree: 'Universitärer Abschluss',
  // languages_question: 'Muttersprache',
  // languages_more: '(in der Regel die Sprache, die Sie als Kind zu Hause gesprochen haben)',
  // comments_question: 'Weitere Kommentare'
});

// The 'thanks' view is crucial; never delete it; it submits the results!
const thanks = magpieViews.view_generator("thanks", {
  trials: 1,
  name: "thanks",
  title: "Thank you for taking part in this experiment!",
  prolificConfirmText: "Press the button"
});

// test phase trials
const test_multiple_slider = magpieViews.view_generator(
  "slider_rating", {
    // This will use all trials specified in `data`, you can user a smaller value (for testing), but not a larger value
    trials: 1,
    // name should be identical to the variable name
    name: "slider_test",
    data: example_trial
  },
  // you can add custom functions at different stages through a view's life cycle
  {
    stimulus_container_generator: multi_slider_generator.stimulus_container_gen,
    answer_container_generator: multi_slider_generator.answer_container_gen,
    handle_response_function: multi_slider_generator.handle_response_function
  }
);

// experimental phase trials
const multiple_slider = magpieViews.view_generator(
  "slider_rating", {
    // This will use all trials specified in `data`, you can user a smaller value (for testing), but not a larger value
    trials: slider_rating_trials.length,
    // name should be identical to the variable name
    name: "slider_main",
    data: _.shuffle(slider_rating_trials)
  },
  // you can add custom functions at different stages through a view's life cycle
  {
    stimulus_container_generator: multi_slider_generator.stimulus_container_gen,
    answer_container_generator: multi_slider_generator.answer_container_gen,
    handle_response_function: multi_slider_generator.handle_response_function
  }
);
