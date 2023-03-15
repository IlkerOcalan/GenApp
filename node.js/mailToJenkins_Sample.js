const nodemailer = require('nodemailer');

// create a nodemailer transporter for sending emails
const transporter = nodemailer.createTransport({
  host: 'your-smtp-host', // replace with your SMTP server hostname
  port: 587, // replace with the SMTP server port
  secure: false, // true for 465, false for other ports
  auth: {
    user: 'your-smtp-username', // replace with your SMTP username
    pass: 'your-smtp-password' // replace with your SMTP password
  }
});

// create a mail options object with the Jenkins build URL and parameters
const mailOptions = {
  from: 'your-email@example.com', // replace with your email address
  to: 'jenkins@example.com', // replace with the Jenkins email address for triggering builds
  subject: 'Trigger Jenkins build',
  text: 'Build URL: https://your-jenkins-host/job/your-job-name/buildWithParameters?param1=value1&param2=value2'
};

// send the email using the nodemailer transporter
transporter.sendMail(mailOptions, (error, info) => {
  if (error) {
    console.error(error);
  } else {
    console.log(`Email sent: ${info.response}`);
  }
});


//In this script, 
//your-smtp-host: the hostname of your SMTP server
//587: the port number of your SMTP server
//your-smtp-username: your SMTP username
//your-smtp-password: your SMTP password
//your-email@example.com: your email address
//jenkins@example.com: the email address used by Jenkins for triggering builds
//your-jenkins-host: the hostname of your Jenkins server
//your-job-name: the name of the Jenkins job to trigger
//param1=value1&param2=value2: the query parameters to pass to the Jenkins job (if any)