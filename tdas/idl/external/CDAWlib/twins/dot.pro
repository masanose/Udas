;  $Source: /usr/lib/cvsroot/twins-idl/lib/lanl/dot.pro,v $
;  $Revision: 7091 $
;  $Date: 2010-01-12 12:07:14 -0800 (Tue, 12 Jan 2010) $

function dot, x, y

;+
;  Purpose:
;	
;  Arguments:
;  Preconditions:
;  Postconditions:
;  Invariants:
;  Example:
;  Notes:
;
;  Author:	Pontus Brandt at APL?
;  Modification $Author: jimm $
;-
 Compile_Opt StrictArr
 return, total(x*y)
end
