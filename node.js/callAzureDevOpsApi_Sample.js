const https = require('https');

const azureUrl = 'https://dev.azure.com/your_organization/your_project/_apis/pipelines';
const azureToken = 'your_azure_personal_access_token';
const pipelineId = 'your_pipeline_id';

const options = {
    hostname: 'dev.azure.com',
    path: `/your_organization/your_project/_apis/pipelines/${pipelineId}/runs?api-version=6.0-preview.1`,
    method: 'POST',
    headers: {
        'Authorization': `Basic ${Buffer.from(`:${azureToken}`).toString('base64')}`,
    },
    rejectUnauthorized: false,
};

const req = https.request(options, res => {
    console.log(`statusCode: ${res.statusCode}`);
});

req.on('error', error => {
    console.error(error);
});

req.end();

//The azureToken variable should be set to the appropriate personal access token for your Azure DevOps organization.
//The rejectUnauthorized option is set to false to allow self-signed SSL certificates


