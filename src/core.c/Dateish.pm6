my role Dateish {
    has int $.year;
    has int $.month;
    has int $.day;
    has int $.daycount;
    has     &.formatter;

    method IO(Dateish:D: --> IO::Path:D) {  # because Dateish is not Cool
        IO::Path.new(~self)
    }

    method CALL-ME(Dateish:U: \dateish) { self.new(dateish) }

    # this sub is also used by DAYS-IN-MONTH, which is used by other types
    sub IS-LEAP-YEAR(int $y --> Bool:D) {
        $y %% 4 and not $y %% 100 or $y %% 400
    }
    method is-leap-year(Dateish:D: --> Bool:D) { IS-LEAP-YEAR($!year) }

    my constant $days-in-month = nqp::list_i(
      0, 31, 0, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
    );
    # This method is used by Date and DateTime:
    method !DAYS-IN-MONTH(\year, \month --> Int:D) {
        nqp::atpos_i($days-in-month,month) ||
          ( month == 2 ?? 28 + IS-LEAP-YEAR(year) !! Nil );
    }
    method days-in-month(Dateish:D: --> Int:D) {
        self!DAYS-IN-MONTH($!year,$!month)
    }

    method !year-Str(--> Str:D) {
        sprintf 0 <= $!year <= 9999 ?? '%04d' !! '%+05d', $!year;
    }

    # noop for subclasses
    method !SET-DAYCOUNT() { self }

    # shortcut for out of range throwing
    method !oor($what, $got, $range) {
        X::OutOfRange.new(:$what, :$got, :$range).throw
    }

    # shortcut for invalid format throwing
    method !tif($invalid-str, $target, $format) {
        X::Temporal::InvalidFormat.new(:$invalid-str,:$target,:$format).throw
    }

    multi method new(Dateish:) {
        Failure.new(
            "Cannot call {self.^name}.new with "
                ~ (%_ ?? "these named parameters: {%_.keys}" !! "no parameters")
        )
    }

    multi method Str(Dateish:D: --> Str:D) {
        &!formatter ?? &!formatter(self) !! self!formatter
    }
    multi method gist(Dateish:D: --> Str:D) { self.Str }

    method daycount(--> Int:D) {
        $!daycount
          ?? $!daycount
          !! ($!daycount = self!calculate-daycount)
    }
    method !calculate-daycount() {
        # taken from <http://www.merlyn.demon.co.uk/daycount.htm>
        my int $y = $!year;
        my int $m = $!month;
        nqp::if(
          nqp::islt_i($m,3),
          nqp::stmts(
            ($y = nqp::sub_i($y,1)),
            ($m = nqp::add_i($m,12))
          )
        );

        -678973 + $!day + (153 * $m - 2) div 5
          + 365 * $y + $y div 4
          - $y div 100  + $y div 400
    }

    method !ymd-from-daycount(int $daycount, \year, \month, \day --> Nil) {
        # taken from <http://www.merlyn.demon.co.uk/daycount.htm>
        my Int $dc = $daycount + 678881;
        my Int $ti = (4 * ($dc + 36525)) div 146097 - 1;
        my Int $year = 100 * $ti;
        my int $day = $dc - (36524 * $ti + ($ti div 4));
        my int $t = (4 * ($day + 366)) div 1461 - 1;
        year = $year + $t;
        $day  = $day - (365 * $t + ($t div 4));
        my int $month = (5 * $day + 2) div 153;
        day = $day - ((2 + $month * 153) div 5 - 1);
        if ($month > 9) {
            month = $month - 9;
            year  = year + 1;
        }
        else {
            month = $month + 3;
        }
    }

    method day-of-month(--> Int:D) { $!day }
    method day-of-week(Dateish:D:--> Int:D) { (self.daycount + 2) % 7 + 1 }

    method week() { # algorithm from Claus Tøndering
        my int $a = $!year - ($!month <= 2).floor.Int;
        my int $b = $a div 4 - $a div 100 + $a div 400;
        my int $c = ($a - 1) div 4 - ($a - 1) div 100 + ($a - 1) div 400;
        my int $s = $b - $c;
        my int $e = $!month <= 2 ?? 0 !! $s + 1;
        my int $f = $!day
          + ($!month <= 2
              ?? 31*($!month - 1) - 1
              !! (153*($!month - 3) + 2) div 5 + 58 + $s);

        my int $g = ($a + $b) % 7;
        my int $d = ($f + $g - $e) % 7;
        my int $n = $f + 3 - $d;

           $n < 0        ?? ($!year - 1, 53 - ($g - $s) div 5)
        !! $n > 364 + $s ?? ($!year + 1, 1                   )
        !!                  ($!year,     $n div 7 + 1        );
    }
    method week-year(--> Int:D)   { self.week.AT-POS(0) }
    method week-number(--> Int:D) { self.week.AT-POS(1) }

    method weekday-of-month(--> Int:D) {
        ($!day - 1) div 7 + 1
    }

    my constant $days-at-start-of-month = nqp::list_i(
      0, 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334
    );
    method day-of-year(--> Int:D) {
        $!day
          + nqp::atpos_i($days-at-start-of-month,$!month)
          + ($!month > 2 && IS-LEAP-YEAR($!year));
    }

    method yyyy-mm-dd(str $sep = "-" --> Str:D) {
        my $parts := nqp::list_s;
        nqp::push_s($parts, $!year < 1000 || $!year > 9999
          ?? self!year-Str
          !! nqp::tostr_I(nqp::getattr_i(self,$?CLASS,'$!year'))
        );
        nqp::push_s($parts,
          nqp::concat(nqp::x('0',nqp::islt_i($!month,10)),$!month)
        );
        nqp::push_s($parts,
          nqp::concat(nqp::x('0',nqp::islt_i($!day,10)),$!day)
        );
        nqp::join($sep,$parts)
    }

    method dd-mm-yyyy(str $sep = "-" --> Str:D) {
        my $parts := nqp::list_s;
        nqp::push_s($parts,
          nqp::concat(nqp::x('0',nqp::islt_i($!day,10)),$!day)
        );
        nqp::push_s($parts,
          nqp::concat(nqp::x('0',nqp::islt_i($!month,10)),$!month)
        );
        nqp::push_s($parts, $!year < 1000 || $!year > 9999
          ?? self!year-Str
          !! nqp::tostr_I(nqp::getattr_i(self,$?CLASS,'$!year'))
        );
        nqp::join($sep,$parts)
    }

    method mm-dd-yyyy(str $sep = "-" --> Str:D) {
        my $parts := nqp::list_s;
        nqp::push_s($parts,
          nqp::concat(nqp::x('0',nqp::islt_i($!month,10)),$!month)
        );
        nqp::push_s($parts,
          nqp::concat(nqp::x('0',nqp::islt_i($!day,10)),$!day)
        );
        nqp::push_s($parts, $!year < 1000 || $!year > 9999
          ?? self!year-Str
          !! nqp::tostr_I(nqp::getattr_i(self,$?CLASS,'$!year'))
        );
        nqp::join($sep,$parts)
    }

    proto method earlier(|) {*}
    multi method earlier(Dateish:D: *%unit --> Dateish:D) {
        my $units := nqp::getattr(%unit,Map,'$!storage');
        nqp::iseq_i(nqp::elems($units),1)
          ?? self.move-by-unit(
               (my str $u = nqp::iterkey_s(nqp::shift(nqp::iterator($units)))),
               -nqp::atkey($units,$u)  # must be HLL negation
             )
          !! self!move-die(nqp::elems($units))
    }
    multi method earlier(Dateish:D: @pairs) {
        my $dateish  := self;
        my int $elems = @pairs.elems;   # reifies
        my $reified  := nqp::getattr(@pairs,List,'$!reified');
        my int $i     = -1;
        nqp::while(
          nqp::bitand_i(
            nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
            nqp::istype((my $pair := nqp::decont(nqp::atpos($reified,$i))),Pair)
          ),
          $dateish := $dateish.move-by-unit(
            nqp::getattr($pair,Pair,'$!key'),
            -nqp::getattr($pair,Pair,'$!value')  # must be HLL negation
          )
        );

        nqp::islt_i($i,$elems)
          ?? (die "Can not use a {$pair.raku} as a time unit")
          !! $dateish
    }

    proto method later(|) {*}
    multi method later(Dateish:D: *%unit --> Dateish:D) {
        my $units := nqp::getattr(%unit,Map,'$!storage');
        nqp::iseq_i(nqp::elems($units),1)
          ?? self.move-by-unit(
               (my str $u = nqp::iterkey_s(nqp::shift(nqp::iterator($units)))),
               nqp::atkey($units,$u)
             )
          !! self!move-die(nqp::elems($units))
    }
    multi method later(Dateish:D: @pairs) {
        my $dateish  := self;
        my int $elems = @pairs.elems;   # reifies
        my $reified  := nqp::getattr(@pairs,List,'$!reified');
        my int $i     = -1;
        nqp::while(
          nqp::bitand_i(
            nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
            nqp::istype((my $pair := nqp::decont(nqp::atpos($reified,$i))),Pair)
          ),
          $dateish := $dateish.move-by-unit(
            nqp::getattr($pair,Pair,'$!key'),
            nqp::getattr($pair,Pair,'$!value')
          )
        );

        nqp::islt_i($i,$elems)
          ?? (die "Can not use a {$pair.raku} as a time unit")
          !! $dateish
    }

    # die for improper number of units when moving a Dateish
    method !move-die(int $elems) {
        die $elems
          ?? "More than one time unit supplied. Please provide these as a List of Pairs to indicate order of application if this is intended.".naive-word-wrapper
          !! die "No time unit supplied";
    }

    my int constant PAD-SPACE  = 0;
    my int constant PAD-ZERO   = 1;
    my int constant PAD-PLUS   = 2;
    my int constant NO-PAD     = 3;
    my int constant UPPERCASE  = 4;
    my int constant LOWERCASE  = 5;
    # my int constant LOCAL-E    = 6; unused in current implementation
    # my int constant LOCAL-O    = 7; unused in current implementation

    # Putting digits in a hash makes for easy comparison avoiding combining characters and makes math easier.
    my constant padding   = nqp::hash('0', 0, '1', 1, '2', 2, '3', 3, '4', 4, '5', 5, '6', 6, '7', 7, '8', 8, '9', 9);
    my constant modifiers = nqp::hash('0', PAD-ZERO, '_', PAD-SPACE, '+', PAD-PLUS, '-', NO-PAD, '^', UPPERCASE, '#', LOWERCASE);
    my constant a_fmt  = nqp::list('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday','Sunday');
    my constant b_fmt  = nqp::list('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September',
            'October', 'November', 'December');

    # Helper sub that handles padding and transformations.
    my sub inner-fmt(str $text, int $modifier, int $width) {
        nqp::iseq_i($modifier, NO-PAD)
            ?? $text
            !! nqp::iseq_i($modifier, PAD-ZERO)
                ?? nqp::stmts(
                        (my int $pad-width = nqp::sub_i($width,nqp::chars($text))),
                        nqp::iseq_s(substr($text,0,1),'-')
                            ?? nqp::concat(
                                '-',
                                nqp::concat(
                                    nqp::x('0',(nqp::islt_i($pad-width,0) ?? 0 !! $pad-width)),
                                    nqp::substr($text,1)))
                            !! nqp::concat(nqp::x('0',(nqp::islt_i($pad-width,0) ?? 0 !! $pad-width)), $text))
                !! nqp::iseq_i($modifier, PAD-SPACE)
                    ?? nqp::stmts(
                            ($pad-width = nqp::sub_i($width,nqp::chars($text))),
                            nqp::concat(nqp::x(' ',(nqp::islt_i($pad-width,0) ?? 0 !! $pad-width)), $text))
                    !! nqp::iseq_i($modifier, PAD-PLUS) # TODO: and does not begin wih –
                        ?? nqp::concat(
                                nqp::if(nqp::isle_i(nqp::chars($text),$width)
                                     || nqp::iseq_s(nqp::substr($text,0,1),'-')
                                         ,'',
                                         '+'),
                                $text)
                        !! nqp::iseq_i($modifier, UPPERCASE)
                            ?? nqp::uc($text)
                            !! nqp::iseq_i($modifier, LOWERCASE)
                                ?? nqp::lc($text)
                                !! '' # return blank if bad modifier
    }
    # Nano is not a POSIX one, but is a useful one found in some standards.
    # Its formatting is done radically different from others, as width dictates precision.
    my sub fmt-nano(Dateish $d, int $width) {
        my num $s = nqp::tryfindmethod($d, 'second')
                ?? nqp::callmethod(nqp::callmethod($d, 'second'),'Num')
                !! 0;
        nqp::substr(
                nqp::sprintf(
                        '%.*f',
                        nqp::list(
                                nqp::coerce_is($width),
                                nqp::sub_n($s,nqp::floor_n($s)))),
                2,
                $width)
    }

    # The default widths here mostly follow what everyone else does.
    # Default padding is always zero, unless specified differently by the formatter
    # Check that negative years format correctly.
    #
    # Any format NOT directly used in Datish (but commonly used like in DateTime)
    # should use an iffy accessor (.?foo) and provide a default value.
    # s, and year times may be negative
    my constant formats = nqp::hash(
        'a', # Abbreviated day of the week, word-based
        nqp::list(NO-PAD, 0, {
                nqp::substr(
                    nqp::without(
                        nqp::atpos(a_fmt,nqp::sub_i(nqp::callmethod($_,'day-of-week'),1)),
                        ''),
                    0,3)
        }),
        'A', # Day of the week, word-based
        nqp::list(NO-PAD, 0, {
                nqp::without(
                    nqp::atpos(a_fmt,nqp::sub_i(nqp::callmethod($_,'day-of-week'),1)),
                    '')
        }),
        'b', # Abbreviated month of the year, word-based
        nqp::list(NO-PAD, 0, {
                nqp::substr(
                    nqp::without(
                        nqp::atpos(b_fmt, nqp::sub_i(nqp::callmethod($_,'month'), 1)),
                        ''),
                    0,3)
        }),
        'B', # Month of the year, word-based
        nqp::list(NO-PAD, 0, {
                nqp::without(
                    nqp::atpos(b_fmt, nqp::sub_i(nqp::callmethod($_,'month'), 1)),
                    '')
        }),
        'c', # Extended format
        nqp::list(NO-PAD, 0, {
                nqp::callmethod($_,'fmt','%a %b %e %H:%M:%S %Y')
        }),
        'C', # Century
        nqp::list(PAD-ZERO, 2, {
                nqp::coerce_is(nqp::div_i(nqp::callmethod($_,'year'),100))
        }),
        'd', # Day as digits
        nqp::list(PAD-ZERO, 2, {
                nqp::coerce_is(nqp::callmethod($_,'day'))
        }),
        'D', # Month-day-year, slash separated
        nqp::list(NO-PAD, 0, {
                nqp::callmethod($_,'fmt','%m/%d/%y')
        }),
        'e', # Day as digit(s), spaced padded
        nqp::list(PAD-SPACE, 2, {
                nqp::coerce_is(nqp::callmethod($_,'day'))
        }),
        # Mainly defined in Python's strftime, explicitly defined as microseconds that are left padded.
        # We use 'f' as POSIX uses 'F' for a different one
        'f',
        nqp::list(PAD-ZERO, 6, {
                my num $s = nqp::tryfindmethod($_,'second')
                        ?? nqp::callmethod(nqp::callmethod($_,'second'),'Num')
                        !! 0;
                $s = nqp::mul_n(
                        nqp::sub_n($s,nqp::floor_n($s)),
                        nqp::coerce_in(1000000));
                nqp::sprintf('%0.0f',nqp::list($s));
        }),
        'F', # Year/month/day, hyphen separated
        nqp::list(NO-PAD, 0, {
                nqp::callmethod($_,'fmt','%+4Y-%m-%d')
        }),
        'g', # ISO week year, final two digits
        nqp::list(PAD-ZERO, 2, {
                nqp::coerce_is(nqp::mod_i(nqp::callmethod($_,'week-year'),100))
        }),
        'G', # ISO Week-year, full
        nqp::list(PAD-ZERO, 4, {
                nqp::coerce_is(nqp::callmethod($_,'week-year'))
        }),
        'h', # Abbreviated month, text form; identical to %b
        nqp::list(NO-PAD, 0, {
                nqp::substr(
                    nqp::without(
                        nqp::atpos(b_fmt, nqp::sub_i(nqp::callmethod($_,'month'), 1)),
                        ''),
                    0,3)
        }),
        'H', # Hour of day, twenty-four hour format
        nqp::list(PAD-ZERO, 2, {
                nqp::tryfindmethod($_,'hour')
                        ?? nqp::coerce_is(nqp::callmethod($_,'hour'))
                        !! '0';
        }),
        'I', # Hour of day, twelve hour format
        nqp::list(PAD-ZERO, 2, {
                nqp::tryfindmethod($_,'hour')
                        ?? nqp::coerce_is(nqp::mod_i(nqp::callmethod($_,'hour'),12))
                        !! '12'
        }),
        'j', # Day of year
        nqp::list(PAD-ZERO, 3, {
                nqp::coerce_is(nqp::callmethod($_,'day-of-year'))
        }),
        # k and l are non-POSIX but common
        'k', # Hours of day, twenty-four hour format, space padded
        nqp::list(PAD-SPACE, 2, {
                nqp::tryfindmethod($_,'hour')
                    ?? nqp::coerce_is(nqp::callmethod($_,'hour'))
                    !! '0'
               # nqp::stmts( (my $h := nqp::without(.?hour,0,.hour)),nqp::coerce_is($h))
        }),
        'l', # Hours of day, twelve hour format, space padded.
        nqp::list(PAD-SPACE, 2, {
                nqp::tryfindmethod($_,'hour')
                    ?? nqp::coerce_is(nqp::mod_i(nqp::callmethod($_,'hour'),12))
                    !! '12'
        }),
        # Ruby (and a few others) use L for mi*LL*iseconds
        'L', # Millesconds as whole number
        nqp::list(PAD-ZERO, 3, {
                my num $s = nqp::tryfindmethod($_,'second')
                        ?? nqp::callmethod(nqp::callmethod($_,'second'),'Num')
                        !! 0;
                $s = nqp::mul_n(
                        nqp::sub_n($s,nqp::floor_n($s)),
                        nqp::coerce_in(1000));
                nqp::sprintf('%0.0f',nqp::list($s));
        }),
        'm',
        nqp::list(PAD-ZERO, 2, {
                nqp::coerce_is(nqp::callmethod($_,'month'))
        }),
        'M',
        nqp::list(PAD-ZERO, 2, {
                nqp::tryfindmethod($_,'minute')
                    ?? nqp::coerce_is(nqp::callmethod($_,'minute'))
                    !! '0'
        }),
        'n',
        nqp::list(NO-PAD, 0, {
                "\n"
        }),
        #'N' is special cased, because the width functions as a max, rather than a minimum width
        'p', # AM/PM marker, uppercase
        nqp::list(PAD-ZERO, 2, {
                nqp::islt_i(
                    nqp::tryfindmethod($_,'hour')
                            ?? nqp::callmethod($_,'hour')
                            !! 0,
                    12)
                        ?? 'AM'
                        !! 'PM'
        }),
        # Not POSIX, but common
        'P', # AM/PM marker, lowercase
        nqp::list(PAD-ZERO, 2, {
                nqp::islt_i(
                    nqp::tryfindmethod($_,'hour')
                            ?? nqp::callmethod($_,'hour')
                            !! 0,
                    12)
                        ?? 'am'
                        !! 'pm'
        }),
        'r',
        nqp::list(NO-PAD, 0, {
                nqp::callmethod($_, 'fmt','%I:%M:%S %p')
        }),
        # Not POSIX, but common
        'R', nqp::list(NO-PAD, 0, {
                nqp::callmethod($_, 'fmt','%H:%M')
        }),
        's', # Seconds from UNIX epoch
        nqp::list(PAD-ZERO, 1, {
                nqp::coerce_is(nqp::callmethod($_,'posix'))
        }),
        'S', # Seconds in minute
        nqp::list(PAD-ZERO, 2, {
                nqp::tryfindmethod($_,'second') # May give [Fat]Rat, in which case we get Rat→Num→int→str
                        ?? nqp::coerce_is(nqp::coerce_ni(nqp::callmethod(nqp::callmethod($_,'second'),'Num')))
                        !! '0'
        }),
        't', # Literal tab
        nqp::list(NO-PAD, 0, {
                "\t"
        }),
        'T', # Hour-minute-second format, colon separated
        nqp::list(NO-PAD, 0, {
                nqp::callmethod($_,'fmt','%H:%M:%S')
        }),
        'u', # Day of the week, 1 (Monday) - 7 (Sunday)
        nqp::list(PAD-ZERO, 1, {
                nqp::coerce_is(nqp::callmethod($_,'day-of-week'))
        }),
        'U', # Week of week-year: 0 (days before first Sunday), 1 (Starts with first Sunday) - 53
        nqp::list(PAD-ZERO, 2, {
                nqp::coerce_is(
                    nqp::div_i(
                        nqp::sub_i(
                            nqp::add_i(nqp::callmethod($_,'day-of-year'),6),
                            nqp::mod_i(nqp::callmethod($_,'day-of-week'),7)),
                        7))
        }),
        'V', # Week of week-year, ISO: 1 (first week with four days in January, Monday-based) - 53
        nqp::list(PAD-ZERO, 2, {
                nqp::coerce_is(nqp::callmethod($_,'week-number'))
        }),
        'w', # Day of week, 0 (Sunday) - 6 (Saturday)
        nqp::list(PAD-ZERO, 1, {
                nqp::coerce_is(nqp::mod_i(nqp::callmethod($_,'day-of-week'),7))
        }),
        'W', # Week of week-year: 0 (days before first Monday), 1 (Starts with first Monday) - 53
        nqp::list(PAD-ZERO, 2, {
                nqp::coerce_is(
                    nqp::div_i(
                        nqp::sub_i(
                            nqp::add_i(nqp::callmethod($_,'day-of-year'),6),
                            nqp::mod_i(
                                nqp::add_i(nqp::callmethod($_,'day-of-week'),6),
                                7)),
                        7))
        }),
        'x', # Month-day-year, slash separated
        nqp::list(NO-PAD, 0, {
                nqp::callmethod($_,'fmt','%m/%d/%y')
        }),
        'X', # Hour-minute-second, colon separated
        nqp::list(NO-PAD, 0, {
                nqp::callmethod($_,'fmt','%H:%M:%S')
        }),
        'y', # Year, final two digits
        nqp::list(PAD-ZERO, 2, {
                nqp::coerce_is(nqp::mod_i(nqp::callmethod($_,'year'),100))
        }),
        'Y', # Year
        nqp::list(PAD-ZERO, 4, {
            nqp::coerce_is(nqp::callmethod($_,'year')) } ),
        'z', # Timezone, offset format
        nqp::list(NO-PAD, 0, {
                my int $z =
                        nqp::tryfindmethod($_,'offset')
                            ?? nqp::callmethod($_,'offset')
                            !! 0;
                # Zero is, per relevant standards, always considered positive
                my str $sign = nqp::islt_i($z,0) ?? '-' !! '+';
                $z = nqp::abs_i($z);
                nqp::concat(
                        $sign,
                        nqp::sprintf(
                            '%02d%02d',
                            nqp::list(
                                nqp::div_i($z,3600),
                                nqp::mod_i(nqp::div_i($z,60),60))))
        }),
        'Z', # Named timezone
        nqp::list(NO-PAD, 0, {
                # Named timezones don't exist per se in Raku core.  However, they could be
                # enabled in module space.  As an extremely rough heuristic to see if we've
                # received a named timezone, we check for the existence of a letter in its
                # stringified form and that string if so.  Otherwise, if we could not
                # determine the timezone *name* per the standard, we return a blank string.
                my $z :=
                        nqp::callmethod(
                                nqp::tryfindmethod($_,'timezone')
                                        ?? nqp::callmethod($_,'timezone')
                                        !! '',
                                'Str');
                $z ~~ /<alpha>/ ?? $z !! ''
                      #           ?? '.?timezone; nqp::without($z, '', ($z ~~ /<alpha>/ ?? $z !! ''))
        }),
        '%', # Literal percent
        nqp::list(NO-PAD, 0, {
                '%'
        }),
        '+', # Extended format (common prior to the + modifier, still in some utilities)
        nqp::list( NO-PAD, 0, {
            nqp::callmethod($_,'fmt','%a %b %e %H:%M:%S %Z %Y')
        })
    );

    method fmt ($in) {
        my str $fmt-str = nqp::unbox_s($in);
        my int $pos     = 0;
        my     $out    := nqp::list_s();
        my int $max     = nqp::chars($fmt-str);

        while nqp::isne_i(-1,(my int $new = nqp::index($fmt-str,'%',$pos))) {
            my int $modifier = -1;
            my int $width    = -1;
            my str $char;
            #my int $local    = -1; unused in current implementation, see note below when parsing

            # Add the literal text up to the '%'
            nqp::push_s($out,nqp::substr($fmt-str, $pos, $new - $pos));
            $pos = $new;
            $new = $new + 1;

            # Check that we can get another character, and then see if it's a modifier
            nqp::if(nqp::islt_i($new, $max),
                nqp::stmts(
                    # We can
                    ($char = nqp::substr($fmt-str, $new, 1)),
                    nqp::if(nqp::existskey(modifiers, $char),
                        # It is a modifier
                        nqp::stmts(
                            ($modifier = nqp::atkey(modifiers, $char)),
                            ($new = nqp::add_i($new, 1)),
                            (nqp::isge_i($new, $max)
                                    ?? (nqp::null)
                                    !! ($char = nqp::substr($fmt-str,$new,1)))))));

            # Check if there's a padding value, double check end of string
            nqp::if(nqp::islt_i($new,$max),
                nqp::stmts(
                    ($char = nqp::substr($fmt-str, $new, 1)),
                    nqp::if(nqp::existskey(padding, $char),
                        nqp::stmts(
                            ($width = nqp::atkey(padding, $char)),
                            ($new = nqp::add_i($new,1)),
                            nqp::while(
                                nqp::stmts(
                                    ($char = nqp::substr($fmt-str,$new,1)),
                                    nqp::existskey(padding, $char)),
                                nqp::stmts(
                                    ($width = nqp::mul_i($width,10)),
                                    ($width = nqp::add_i($width,nqp::atkey(padding, $char))),
                                    ($new = nqp::add_i($new,1))))))));

            # POSIX/C provide for localized versions formatters, indicated
            # by the prefix of an E or an O.  We only provide the POSIX locale,
            # so we just parse past it if we find it.
            nqp::if(nqp::islt_i($new,$max),
                nqp::stmts(
                    ($char = nqp::substr($fmt-str, $new, 1)),
                    nqp::if(nqp::iseq_s($char,'E'),
                        nqp::stmts(
                            ($new = nqp::add_i($new,1)),
                            #`[($local = LOCAL-E)]),
                        nqp::if(nqp::iseq_s($char,'O'),
                            nqp::stmts(
                                ($new = nqp::add_i($new,1)),
                                #`[($local = LOCAL-O)])))));

            nqp::if(nqp::islt_i($new,$max),
                nqp::stmts(
                    ($char = nqp::substr($fmt-str, $new, 1)),
                    nqp::if(nqp::existskey(formats, $char),
                        nqp::stmts(
                            (my $format := nqp::atkey(formats,$char)),
                            nqp::if(nqp::iseq_i($modifier,-1),($modifier = nqp::atpos($format,0))),
                            nqp::if(nqp::iseq_i($width,   -1),($width    = nqp::atpos($format,1))),
                            nqp::push_s(
                                $out,
                                nqp::call(
                                    nqp::getlex('&inner-fmt'), # this sub could be manually inlined here
                                    nqp::call(nqp::atpos($format,2),self),
                                    $modifier,
                                    $width)),
                            ($new = nqp::add_i($new,1))),
                        nqp::if(nqp::iseq_s($char,'N'),
                            nqp::stmts( # Special case fractional seconds because they format differently
                                nqp::push_s(
                                    $out,
                                    nqp::call(
                                        nqp::getlex('&fmt-nano'),
                                        self,
                                        (nqp::iseq_i($width,-1) ?? 9 !! $width))), # default to 9 for *n*anoseconds
                                ($new = nqp::add_i($new,1))),
                            # Bad format.  POSIX/ISO C lists as undefined behavior.
                            # Implementations are split on how to best handle this.
                            # Some include the %, some only the text after it.
                            # I've seen several justifications for both, but most convincing
                            # to me is that if someone unintentionally has a bad format, the
                            # presence of a % indicates a syntax error.  A % that was
                            # mistakenly not escaped will still display as (likely) intended.
                            nqp::push_s($out,nqp::substr($fmt-str,$pos, nqp::sub_i($new,$pos)))))),
                nqp::push_s($out,nqp::substr($fmt-str, $pos)));
            $pos = $new;
        };

        # Add the rest of the format string if we are not at the end.
        nqp::if(
            nqp::isne_i(nqp::chars($fmt-str),$pos),
            nqp::push_s($out, nqp::substr($fmt-str, $pos))
        );

        nqp::join('',$out)
    }
}

# =begin pod
#
# =head1 SEE ALSO
# Raku spec <S32-Temporal|https://design.raku.org/S32/Temporal.html>.
# The Perl DateTime Project home page L<http://datetime.perl.org>.
# Perl perldoc L<doc:DateTime> and L<doc:Time::Local>.
#
# The best yet seen explanation of calendars, by Claus Tøndering
# L<Calendar FAQ|http://www.tondering.dk/claus/calendar.html>.
# Similar algorithms at L<http://www.hermetic.ch/cal_stud/jdn.htm>
# and L<http://www.merlyn.demon.co.uk/daycount.htm>.
#
# L<ISO 8601|http://en.wikipedia.org/wiki/ISO_8601>
# L<Time zones|http://en.wikipedia.org/wiki/List_of_time_zones>
#
# =end pod

# vim: expandtab shiftwidth=4
