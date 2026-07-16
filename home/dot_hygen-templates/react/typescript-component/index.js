module.exports = {
  prompt: async ({ inquirer, args }) => {
    const answers = {};
    const promptIfEmpty = async (props) => {
      if (!(props.name in args)) {
        Object.assign(answers, await inquirer.prompt(props));
      }
    };

    await promptIfEmpty({
      type: "input",
      name: "componentName",
      message: "Component name:",
      validate: (input) => input.length > 0 || "Component name cannot be empty",
    });

    return answers;
  },
};
