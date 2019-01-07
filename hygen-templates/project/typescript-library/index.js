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

    await promptIfEmpty({
      type: 'input',
      name: 'description',
      message: 'Description'
    });

    await promptIfEmpty({
      type: 'multiselect',
      name: 'targets',
      message: 'Target platform(s):',
      choices: [{ name: 'node', message: 'Node' }, { name: 'browser', message: 'Browser' }],
      default: ['Node'],
      validate: answers => answers.length >= 1 || 'Must select at least one'
    });

    await promptIfEmpty({
      type: 'input',
      name: 'license',
      message: 'License',
      default: 'ISC',
      validate: input => {
        const validationResult = validateLicense(input);
        if (validationResult.validForNewPackages) {
          return true;
        }

        return validationResult.warnings.join();
      }
    });

    await promptIfEmpty({
      type: 'input',
      name: 'author',
      message: 'Author'
    });

    await promptIfEmpty({
      type: 'input',
      name: 'repository',
      message: 'Repository'
    });

    await promptIfEmpty({
      type: 'input',
      name: 'nodeVersion',
      message: 'Node version',
      default: process.versions.node
    });

    return answers;
  }
};
