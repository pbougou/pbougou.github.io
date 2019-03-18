function mailto(user, hostname, domain, classname, realname, subject) {
    var mailto = user + '@' + hostname + '.' + domain;
    if (realname != null)
        mailto = "\"" + realname + "\" <" + mailto + ">";
    document.write('<a href="' + 'mailto:' + escape(mailto));
    if (subject != null)
        document.write('?subject=' + escape(subject));
    if (classname != null)
        document.write('" class="' + classname);
    document.write('">' + user + '@' + hostname + '.' + domain + '</a>');
}

function noSPAMfix(id, inner) {
    var a = document.getElementById(id);
    function fixit(s) {
        return s.replace(/%20AT%20/g, "@").replace(/%20DOT%20/g, ".");
    }
    a.href = fixit(a.href);
    if (inner != null)
        a.innerHTML = fixit(a.innerHTML);
}
