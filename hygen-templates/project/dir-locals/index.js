const findUp = require('find-up');
const which = require('npm-which');

module.exports = {
  prompt: async ({ inquirer, args }) => {
    const answers = {};
    const promptIfEmpty = async props => {
      if (!(props.name in args)) {
        Object.assign(answers, await inquirer.prompt(props));
      }
    };

    await promptIfEmpty({
      type: 'multiselect',
      name: 'features',
      message: 'Features',
      choices: [
        { name: 'js', message: 'Javascript' },
        { name: 'ts', message: 'Typescript' },
        { name: 'jsx', message: 'JSX' },
        { name: 'json', message: 'JSON' }
      ],
      initial: ['js', 'ts', 'jsx', 'json']
    });

    return {
      ...answers,
      customHelpers: {
        which: cwd => name => {
          try {
            return which(cwd).sync(name);
          } catch (e) {
            return null;
          }
        },
        findUp: cwd => names => findUp.sync(names, { cwd })
      }
    };
  }
};
