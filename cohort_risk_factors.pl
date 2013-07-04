#!/usr/local/bin/perl

use strict;
use warnings;

my @variables = ("gender","agegroup3","children","mass_contact_children","smoking","transport","risk","employment","vaccinated");

print("pdf(\"cohorts.pdf\", width=10, height=5)\n\n");
for (my $i = 0; $i < (scalar @variables); $i++)
  {
    my $variable = shift @variables;
    system("perl ~/code/epiwork-website/scripts/cohorts/cohorts.pl ".
           "-f ~/code/epiwork-website/scripts/cohorts/variables ".
           "-b postgres ".
           "-t epidb ".
           "-m $variable ".
#           "-d fever ".
           "-o ".join(",", @variables)." ".
           "> $variable.csv");
    push @variables, $variable;

    print("$variable <- data.table(read.csv(\"$variable.csv\", quote=\"''\"))\n");
    print("$variable\$date <- ISOweek2date(paste($variable\$year,\"-W\",sprintf(\"%02d\",$variable\$week),\"-\",4,sep=\"\"))\n");
#    print("pdf(\"cohorts_$variable.pdf\", width=10, height=5)\n");
    print("ggplot($variable\[date >= \"2012-11-22\" & date <= \"2013-04-14\"\], aes(x=date, y=value, color=variable))+\n");
    print("  theme_bw(20)+\n");
    print("  geom_line(lwd=1.5)+\n");
    print("  scale_color_brewer(palette=\"Set1\")+\n");
    print("  theme(legend.title=element_blank(), legend.position=c(0.7,0.8),\n");
    print("        legend.background = element_rect(fill=alpha('white', 0)))+\n");
    print("  scale_y_continuous(\"Incidence\", labels=percent)+\n");
    print("  scale_x_date(\"\")\n");
#    print("dev.off()\n");
    print("\n");
  }
print("dev.off()\n");
