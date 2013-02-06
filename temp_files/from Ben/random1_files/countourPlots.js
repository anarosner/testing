(function($) {
    $(document).ready(function() {
	
	$('#countourPlots').scianimator({
	    'images': ['plots/countourPlots1.png', 'plots/countourPlots2.png', 'plots/countourPlots3.png', 'plots/countourPlots4.png', 'plots/countourPlots5.png', 'plots/countourPlots6.png', 'plots/countourPlots7.png', 'plots/countourPlots8.png', 'plots/countourPlots9.png', 'plots/countourPlots10.png', 'plots/countourPlots11.png', 'plots/countourPlots12.png', 'plots/countourPlots13.png'],
	    'width': 480,
	    'delay': 800,
	    'loopMode': 'loop'
	});
	$('#countourPlots').scianimator('play');
    });
})(jQuery);
