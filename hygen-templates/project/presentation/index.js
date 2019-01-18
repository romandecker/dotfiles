const validateLicense = require('validate-npm-package-license');

module.exports = {
  prompt: async ({ inquirer, args }) => {
    const answers = {};
    const promptIfEmpty = async props => {
      if (!(props.name in args)) {
        Object.assign(answers, await inquirer.prompt(props));
      }
    };

    await promptIfEmpty({
      type: 'input',
      name: 'projectName',
      message: 'Project name:',
      validate: input => input.length > 0 || 'Project name cannot be empty'
    });

    return answers;
  }
};
