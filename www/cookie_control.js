var config = {
    apiKey: '8124333071c2edce46eda1d77d59917c2c363bcf',
    product: 'COMMUNITY',
    text : {
        thirdPartyTitle : 'Warning: Some cookies require your attention',
        thirdPartyDescription : 'Consent for some third party cookies can not be automatically revoked. Please follow the link below if you want to opt out of them.'
    },
    optionalCookies: [
        {
            name : 'analytics',
            label: 'Analytical Cookies',
            description: 'Analytical cookies help us to improve our website by collecting and reporting information on its usage.',
            cookies: ['_ga', '_gid', '_gat', '__utma', '__utmt', '__utmb', '__utmc', '__utmz', '__utmv'],
            onAccept : function(){
                // Add Google Analytics
                window.dataLayer = window.dataLayer || [];
                function gtag(){dataLayer.push(arguments);}
                gtag('js', new Date());
                gtag('config', 'G-PXBPFFZ1RL', { 'anonymize_ip':true });
                // End Google Analytics
            },
            onRevoke: function(){
                // Disable Google Analytics
                window['ga-disable-G-PXBPFFZ1RL'] = true;
                // End Google Analytics
            }
        }
    ]
};

CookieControl.load( config );