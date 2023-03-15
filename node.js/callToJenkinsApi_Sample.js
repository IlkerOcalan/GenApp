const https = require('https');

const jenkinsUrl = 'https://your.jenkins.server.com';
const jenkinsUser = 'your_jenkins_user';
const jenkinsToken = 'your_jenkins_api_token';
const jobName = 'your_pipeline_job_name';

const options = {
    hostname: jenkinsUrl,
    path: `/job/${jobName}/build`,
    method: 'POST',
    auth: `${jenkinsUser}:${jenkinsToken}`,
    rejectUnauthorized: false,
};

const req = https.request(options, res => {
    console.log(`statusCode: ${res.statusCode}`);
});

req.on('error', error => {
    console.error(error);
});

req.end();
