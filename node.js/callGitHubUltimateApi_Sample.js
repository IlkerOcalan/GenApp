const https = require('https');

const owner = 'your-github-username';
const repo = 'your-github-repository';
const workflowId = 'your-github-workflow-id';

const token = 'your-github-personal-access-token';

const options = {
    hostname: 'api.github.com',
    path: `/repos/${owner}/${repo}/actions/workflows/${workflowId}/dispatches`,
    method: 'POST',
    headers: {
        'Accept': 'application/vnd.github.v3+json',
        'Authorization': `Bearer ${token}`,
        'User-Agent': 'node.js'
    }
};

const req = https.request(options, res => {
    console.log(`statusCode: ${res.statusCode}`);
});

req.on('error', error => {
    console.error(error);
});

req.write(JSON.stringify({
    ref: 'main',
    inputs: {}
}));

req.end();


//owner: the GitHub username or organization that owns the repository
//repo: the name of the repository
//workflowId: the ID of the workflow to trigger
//token: your GitHub personal access token with the necessary permissions to trigger workflows