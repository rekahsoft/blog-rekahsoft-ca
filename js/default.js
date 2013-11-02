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
 * Description: Client-side logic for rekahsoft-ca
 */

//------------------------

$(document).ready(function () {
    $.address.init(function(event) {
	console.log("init:");
    }).change(function(event) {
	console.log("change " + event.value);
	
        // remove //pages from current url
	var page_href = event.value;
	console.log(event.value);
	
	if (page_href == '/' || page_href == '') {
	    page_href = '/home.html';
	}
	$('a.menuitem[rel="address:' + page_href + '"]').closest('ul').find('li.active').removeClass('active');
	$('a.menuitem[rel="address:' + page_href + '"]').closest('li').addClass('active');
	
        // set page_href ot full url for ajax call
        page_href = "pages" + page_href;
        
	$.ajax({
	    url: page_href,
	    type: 'GET',
	    dataType: 'html',
	    beforeSend: function (xhr, settings) {
		// Set '#page-content.loading' to show page-content-loading graphic
		$('#page-content').addClass('loading');
		console.log('beforeSend a.menuitem');
	    },
	    success: function (dta) {
		// no need to removeClass('loading'); done on server side
		// use .addClass('fadeIn') to use a css transition *TODO*
		$('#page-content').replaceWith(dta);
	    },
	    error: function (xhr, status) {
		// replace loading graphic with loading error graphic
		$('#page-content').removeClass('loading').addClass('loading-error');
		
		console.log('error retrieving page "' + page_href +'": ' + status);
	    }
	});
    });
    
    $('ul.navbar-nav a.menuitem').click(function() {
	$(this).closest('ul').find('li.active').removeClass('active');
	$(this).closest('li').addClass('active');
	//$('.navbar-collapse').collapse('hide');
    });
    
    // Callback for when the inital page has completely loaded (including images, etc..)
    $(window).load(function () {
        // something
    });
    
    // Load recent-news carousel
    $.ajax({
	url: 'recent-news.html',
	type: 'GET',
	dataType: 'html',
	beforeSend: function (xhr, settings) {
	    // Set '#page-content.loading' to show page-content-loading graphic
	    //$('#page-content').addClass('loading');
	    console.log('Attempting to retrieve recent-news..');
	},
	success: function (dta) {
	    // no need to removeClass('loading'); done on server side
	    // use .addClass('fadeIn') to use a css transition *TODO*
	    $('#page-content').before(dta);
	},
	error: function (xhr, status) {
	    // replace loading graphic with loading error graphic
	    //$('#page-content').removeClass('loading').addClass('loading-error');
	    console.log('Error retrieving recent-news"' + page_href +'": ' + status);
	}
    });
});
