/**
 * (C) Copyright Collin Doering 2013
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/**
 * File:. default.js
 * Author: Collin J. Doering
 * Date: Sep  6, 2013
 * Description: Client-side logic for blog-rekahsoft-ca
 */

//------------------------

(function () {
    var page = (function () {
        var pageId = '#page-content', navId = '#nav';

        function init() {
            $(document).ready(function () {
                $.address.init(function(event) {
                    console.log("init:" + event.value);
                    $(window).load(function () {
                        loadPageContent(event.value);
                    });
                });

                $('#nav-menu a.menuitem').click(function() {
                    $(this).closest('ul').find('li.active').removeClass('active');
                    $(this).closest('li').addClass('active');
                    //$('.navbar-collapse').collapse('hide');
                });

                $('#status a.close-button').click(function () {
                    $(this).parent().slideUp();
                });

                // Callback for when the inital page has completely loaded (including images, etc..)
                $(window).load(function () {
                    $.address.change(function(event) {
                        console.log("change " + event.value);
                        loadPageContent(event.value);
                    });
                });
            });
        }

        function newContentCallback() {
            $('#page-content a').click(function (evt) {
                var page_href = $(this).attr('href');
                var external_url_regexp = /https?:\/\/.*/;
                var mailto_regexp = /mailto:.*/;
                var files_regexp = /files\/.*/;
                var images_regexp = /images\/.*/;

                if (external_url_regexp.test(page_href)
                    || mailto_regexp.test(page_href)
                    || files_regexp.test(page_href)
                    || images_regexp.test(page_href)) {
                    window.location.href = page_href;
                } else if ($(this).attr("rel")) {
                    var virtual_href = $(this).attr('rel').replace(/address:(.*)/, "$1");
                    evt.preventDefault();
                    $.address.value(virtual_href);
                } else {
                    evt.preventDefault();
                    $.address.value(page_href);
                }
            });
        }

        function loadPageContent(page_href) {
            var post_regexp = /posts\/.*/;
            var tag_regexp = /tags\/.*(\d*).html/;
            var blog_page_regexp = /blog\d*.html/;

            // Check whether the requested url is a post
            if (post_regexp.test(page_href)) {
                // Handle post urls (no change required to page_href)
                $('#nav-menu li.active').removeClass('active');
                $('#nav-menu li a[href="./pages/blog.html"]').parent('li').addClass('active');
            } else if (tag_regexp.test(page_href)) {
                // Handle tag pages
                $('#nav-menu li.active').removeClass('active');
                $('#nav-menu li a[href="./pages/blog.html"]').parent('li').addClass('active');

                var tag_not_regexp = /(tags\/.*[^\d]+)(.html)/;
                if (tag_not_regexp.test(page_href))
                    page_href = page_href.replace(tag_not_regexp, "$11$2" );
            } else { // otherwise assume its a page
                // Check if the page_href is empty or / and if so goto home
                if (page_href === '/') {
                    page_href = '/home.html';
                } else if (blog_page_regexp.test(page_href)) {
                    if (page_href === "/blog.html")
                        page_href = "/blog1.html";

                    // If page_href refers to a blog page set Blog to be the active menu item
                    $('a.menuitem[rel="address:/blog.html"]').closest('ul').find('li.active').removeClass('active');
                    $('a.menuitem[rel="address:/blog.html"]').closest('li').addClass('active');
                }

                // Initially set the active menuitem in the nav
                $('a.menuitem[rel="address:' + page_href + '"]').closest('ul').find('li.active').removeClass('active');
                $('a.menuitem[rel="address:' + page_href + '"]').closest('li').addClass('active');

                // set page_href of full url for ajax call
                page_href = "pages" + page_href;
            }

            // Make the ajax request for the new page-content (whether it be a page or a post) *could change*
            $.ajax({
                url: page_href,
                type: 'GET',
                dataType: 'html',
                beforeSend: function (xhr, settings) {
                    // Remove loading error from page-content and any status message errors
                    $('#page-content').removeClass('loading-error');
                    $('#status').slideUp('normal', function () {
                        $('#status').removeClass('error').removeClass('success');
                    });

                    // Add .loading to #page-content and #nav to facilitate a loading animation
                    $('#page-content, #nav').removeClass('loading-done').addClass('loading');

                    console.log('beforeSend a.menuitem');
                },
                success: function (dta) {
                    // Remove the initial loading gif (if its there)
                    $('#page-content').removeClass('init');

                    // Stop animations in the nav and page-content and scroll to the top of the page in a set amount of time
                    setTimeout(function () {
                        // Replace old page-content with new page-content
                        $('#page-content').html(dta);

                        // Stop page loading
                        $('#page-content, #nav').removeClass('loading');

                        // Reload any new maths using MathJax
                        $('#page-content .math').each(function (math_elem) {
                            MathJax.Hub.Queue(["Typeset",MathJax.Hub,math_elem[0]]);
                        });

                        // Scroll to top of the page
                        if ($('body').scrollTop() > $('#nav').offset().top - 15) {
                            $('html, body').animate({
                                scrollTop: $('#nav').offset().top - 15
                            }, 'fast');
                        }

                        // Add new content callbacks
                        newContentCallback();
                    }, 250);
                },
                error: function (xhr, status) {
                    /* Remove .loading from #page-content and #nav to stop the loading
                     * animation. Then add .loading-error to #page-content if its the sites
                     * first load (#page-content has class .init). Finally, display an error
                     * message in #status.
                     */
                    $('#page-content, #nav').removeClass('loading');
                    if ($('#page-content.init')[0]) {
                        $('#page-content').addClass('loading-error').html('<p class="container border-box">Error initially loading blog.rekahsoft.ca. Check the url! Given "' + page_href + '"</p>');
                    } else {
                        $('#status > p.message').text('Error retrieving page "' + page_href +'": ' + status);
                        $('#status').addClass('error').slideDown();
                    }
                }
            });
        }

        return {
            init: init,
            newContentCallback: newContentCallback,
            loadPageContent: loadPageContent
        };
    }());

    page.init();
}());
