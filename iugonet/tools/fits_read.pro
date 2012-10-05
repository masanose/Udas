PRO fits_read,file_or_fcb,data,header,group_par,noscale=noscale, $
    exten_no=exten_no, extname=extname, $
    extver=extver, extlevel=extlevel, xtension=xtension, $
    no_abort=no_abort, message=message, first=first, last=last, $
    group=group, header_only=header_only,data_only=data_only, $
    no_pdu=no_pdu, enum = enum, no_unsigned = no_unsigned, pdu=pdu
    
  ;+
  ; NAME:
  ;       FITS_READ
  ; PURPOSE:
  ;       To read a FITS file.
  ;
  ; CALLING SEQUENCE:
  ;       FITS_READ, filename_or_fcb, data [,header, group_par]
  ;
  ; INPUTS:
  ;       FILENAME_OR_FCB - this parameter can be the FITS Control Block (FCB)
  ;               returned by FITS_OPEN or the file name of the FITS file.  If
  ;               a file name is supplied, FITS_READ will open the file with
  ;               FITS_OPEN and close the file with FITS_CLOSE before exiting.
  ;               When multiple extensions are to be read from the file, it is
  ;               more efficient for the user to call FITS_OPEN and leave the
  ;               file open until all extensions are read. FPACK
  ;               ( http://heasarc.gsfc.nasa.gov/fitsio/fpack/ ) compressed FITS
  ;               files can be read provided that the FPACK software is installed.
  ;
  ; OUTPUTS:
  ;       DATA - data array.  If /NOSCALE is specified, BSCALE and BZERO
  ;               (if present in the header) will not be used to scale the data.
  ;               If Keywords FIRST and LAST are used to read a portion of the
  ;               data or the heap portion of an extension, no scaling is done
  ;               and data is returned as a 1-D vector. The user can use the IDL
  ;               function REFORM to convert the data to the correct dimensions
  ;               if desired.  If /DATA_ONLY is specified, no scaling is done.
  ;       HEADER - FITS Header.  The STScI inheritance convention is recognized
  ;               http://fits.gsfc.nasa.gov/registry/inherit/fits_inheritance.txt
  ;               If an extension is read, and the INHERIT keyword exists with a
  ;               value of T, and the /NO_PDU keyword keyword is not supplied,
  ;               then the primary data unit header and the extension header will
  ;                be combined.  The header will have the form:
  ;
  ;                       <required keywords for the extension: XTENSION, BITPIX,
  ;                               NAXIS, ...>
  ;                       BEGIN MAIN HEADER --------------------------------
  ;                       <PDU header keyword and history less required keywords:
  ;                               SIMPLE, BITPIX, NAXIS, ...>
  ;                       BEGIN EXTENSION HEADER ---------------------------
  ;                       <extension header less required keywords that were
  ;                               placed at the beginning of the header.
  ;                       END
  ;
  ;               The structure of the header is such that if a keyword is
  ;               duplicated in both the PDU and extension headers, routine
  ;               SXPAR will print a warning and return the extension value of
  ;               the keyword.
  ;
  ;       GROUP_PAR - Group parameter block for FITS random groups format files
  ;               or the heap area for variable length binary tables.
  ;               Any scale factors in the header (PSCALn and PZEROn) are not
  ;               applied to the group parameters.
  ;
  ; INPUT KEYWORD PARAMETERS:
  ;
  ;       /NOSCALE: Set to return the FITS data without applying the scale
  ;               factors BZERO and BSCALE.
  ;       /HEADER_ONLY: set to read the header only.
  ;       /DATA_ONLY: set to read the data only.  If set, if any scale factors
  ;               are present (BSCALE or BZERO), they will not be applied.
  ;       /NO_PDU: By default, FITS_READ will add the primary data unit header
  ;               keywords to the output header, *if* the header includes
  ;               INHERIT = T.   Set /NO_PDU to never append the primary header.
  ;       /NO_ABORT: Set to return to calling program instead of a RETALL
  ;               when an I/O error is encountered.  If set, the routine will
  ;               return  a non-null string (containing the error message) in the
  ;               keyword MESSAGE.    (For backward compatibility, the obsolete
  ;               system variable !ERR is also set to -1 in case of an error.)
  ;               If /NO_ABORT not set, then FITS_READ will print the message and
  ;               issue a RETALL
  ;       /NO_UNSIGNED - By default, if  the header indicates an unsigned integer
  ;              (BITPIX = 16, BZERO=2^15, BSCALE=1) then FITS_READ will output
  ;               an IDL unsigned integer data type (UINT).   But if /NO_UNSIGNED
  ;               is set, then the data is converted to type LONG.
  ;       /PDU - If set, then always add the primary data unit header keywords
  ;              to the output header, even if the INHERIT=T keyword is not found
  ;              This was the default behavior of FITS_READ prior to April 2007
  ;       EXTEN_NO - extension number to read.  If not set, the next extension
  ;               in the file is read.  Set to 0 to read the primary data unit.
  ;       XTENSION - string name of the xtension to read
  ;       EXTNAME - string name of the extname to read
  ;       EXTVER - integer version number to read
  ;       EXTLEVEL - integer extension level to read
  ;       FIRST - set this keyword to only read a portion of the data.  It gives
  ;               the first word of the data to read
  ;       LAST - set this keyword to only read a portion of the data.  It gives
  ;               the last word number of the data to read
  ;       GROUP - group number to read for GCOUNT>1.  (Default=0, the first group)
  ;
  ; OUTPUT KEYWORD PARAMETERS:
  ;       ENUM - Output extension number that was read.
  ;       MESSAGE = value: Output error message
  ;
  ; NOTES:
  ;       Determination or which extension to read.
  ;               case 1: EXTEN_NO specified. EXTEN_NO will give the number of the
  ;                       extension to read.  The primary data unit is refered
  ;                       to as extension 0. If EXTEN_NO is specified, XTENSION,
  ;                       EXTNAME, EXTVER, and EXTLEVEL parameters are ignored.
  ;               case 2: if EXTEN_NO is not specified, the first extension
  ;                       with the specified XTENSION, EXTNAME, EXTVER, and
  ;                       EXTLEVEL will be read.  If any of the 4 parameters
  ;                       are not specified, they will not be used in the search.
  ;                       Setting EXTLEVEL=0, EXTVER=0, EXTNAME='', or
  ;                       XTENSION='' is the same as not supplying them.
  ;               case 3: if none of the keyword parameters, EXTEN_NO, XTENSION,
  ;                       EXTNAME, EXTVER, or EXTLEVEL are supplied.  FITS_READ
  ;                       will read the next extension in the file.  If the
  ;                       primary data unit (PDU), extension 0, is null, the
  ;                       first call to FITS_READ will read the first extension
  ;                       of the file.
  ;
  ;               The only way to read a null PDU is to use EXTEN_NO = 0.
  ;
  ;       If FIRST and LAST are specified, the data is returned without applying
  ;       any scale factors (BSCALE and BZERO) and the data is returned in a
  ;       1-D vector.  This will allow you to read any portion of a multiple
  ;       dimension data set.  Once returned, the IDL function REFORM can be
  ;       used to place the correct dimensions on the data.
  ;
  ;       IMPLICIT IMAGES: FITS_READ will construct an implicit image
  ;               for cases where NAXIS=0 and the NPIX1, NPIX2, and PIXVALUE
  ;               keywords are present.  The output image will be:
  ;                       image = replicate(PIXVALUE,NPIX1,NPIX2)
  ;
  ;      FPACK compressed files are always closed and reopened when exiting
  ;      FITS_READ so that the pointer is set to the beginning of the file. (Since
  ;      FPACK files are opened with a bidirectional pipe rather than OPEN, one
  ;      cannot use POINT_LUN to move to a specified position in the file.)
  ;
  ; EXAMPLES:
  ;       Read the primary data unit of a FITS file, if it is null read the
  ;       first extension:
  ;               FITS_READ, 'myfile.fits', data, header
  ;
  ;       Read the first two extensions of a FITS file and the extension with
  ;       EXTNAME = 'FLUX' and EXTVER = 4
  ;               FITS_OPEN, 'myfile.fits', fcb
  ;               FITS_READ, fcb,data1, header2, exten_no = 1
  ;               FITS_READ, fcb,data1, header2, exten_no = 2
  ;               FITS_READ, fcb,data3, header3, extname='flux', extver=4
  ;               FITS_CLOSE, fcb
  ;
  ;       Read the sixth image in a data cube for the fourth extension.
  ;
  ;               FITS_OPEN, 'myfile.fits', fcb
  ;               image_number = 6
  ;               ns = fcb.axis(0,4)
  ;               nl = fcb.axis(1,4)
  ;               i1 = (ns*nl)*(image_number-1)
  ;               i2 = i2 + ns*nl-1
  ;               FITS_READ,fcb,image,header,first=i1,last=i2
  ;               image = reform(image,ns,nl,/overwrite)
  ;               FITS_CLOSE
  ;
  ; PROCEDURES USED:
  ;       FITS_CLOSE, FITS_OPEN
  ;       SXADDPAR, SXDELPAR, SXPAR()
  ; WARNINGS:
  ;       In Sep 2006, FITS_OPEN was modified to open FITS files using the
  ;       /SWAP_IF_LITTLE_ENDIAN keyword to OPEN, so that subsequent routines
  ;       (FITS_READ, FITS_WRITE) did not require any byte swapping.    An error
  ;       may result if an pre-Sep 2006 version of FITS_OPEN is used with a
  ;       post Sep 2006 version of FITS_READ, FITS_WRITE or MODFITS.
  ; HISTORY:
  ;       Written by:     D. Lindler, August 1995
  ;       Avoid use of !ERR       W. Landsman   August 1999
  ;       Read unsigned datatypes, added /no_unsigned   W. Landsman December 1999
  ;       Don't call FITS_CLOSE unless fcb is defined   W. Landsman January 2000
  ;       Set BZERO = 0 for unsigned integer data   W. Landsman  January 2000
  ;       Only call IEEE_TO_HOST if needed          W. Landsman February 2000
  ;       Ensure EXTEND keyword in primary header   W. Landsman April 2001
  ;       Don't erase ERROR message when closing file  W. Landsman April 2002
  ;       Assume at least V5.1 remove NANValue keyword  W. Landsman November 2002
  ;       Work with compress files (read file size from fcb),
  ;       requires updated (Jan 2003) version of FITS_OPEN W. Landsman Jan 2003
  ;       Do not modify BSCALE/BZERO for  unsigned integers W. Landsman April 2006
  ;       Asuume FITS_OPEN has opened the file with /SWAP_IF_LITTLE_ENDIAN
  ;                         W. Landsman   September 2006
  ;       Fix problem with /DATA_ONLY keyword  M.Buie/W.Landsman  October 2006
  ;       Only append primary header if INHERIT=T  W. Landsman  April 2007
  ;       Make ndata 64bit for very large files E. Hivon/W. Landsman May 2007
  ;       Added /PDU keyword to always append primary header W. Landsman June 2007
  ;       Use PRODUCT to compute # of data points   W. Landsman  May 2009
  ;       Make sure FIRST is long64 when computing position W.L. October 2009
  ;       Read FPACK compressed files, W.L.  December 2010
  ;-
  ;
  ;-----------------------------------------------------------------------------
  COMPILE_OPT idl2
  ; print calling sequence
  ;
  IF N_PARAMS() EQ 0 THEN BEGIN
    PRINT,'Syntax - FITS_READ,file_or_fcb,data,header,group_par'
    PRINT,' Input Keywords: /noscale, exten_no=, extname=, '
    PRINT,'               extver=, extlevel=, xtension=, /no_abort, '
    PRINT,'               first, last, group, /header_only, /no_pdu, /pdu'
    PRINT,' Output Keywords: enum =, message='
    RETURN
  ENDIF
  ;
  ; I/O error processing
  ;
  ON_IOERROR,ioerror
  ;
  ; set defaults
  ;
  message = ''
  IF N_ELEMENTS(noscale) EQ 0 THEN noscale = 0
  IF N_ELEMENTS(exten_no) EQ 0 THEN exten_no = -1
  IF N_ELEMENTS(extname) EQ 0 THEN extname = ''
  IF N_ELEMENTS(extver) EQ 0 THEN extver = 0
  IF N_ELEMENTS(extlevel) EQ 0 THEN extlevel = 0
  IF N_ELEMENTS(first) EQ 0 THEN first = 0
  IF N_ELEMENTS(last) EQ 0 THEN last = 0
  IF N_ELEMENTS(no_abort) EQ 0 THEN no_abort = 0
  IF N_ELEMENTS(group) EQ 0 THEN group = 0
  IF N_ELEMENTS(header_only) EQ 0 THEN header_only = 0
  IF N_ELEMENTS(data_only) EQ 0 THEN data_only = 0
  IF N_ELEMENTS(no_pdu) EQ 0 THEN no_pdu = 0
  IF N_ELEMENTS(pdu) EQ 0 THEN pdu = 0
  IF N_ELEMENTS(xtension) EQ 0 THEN xtension = ''
  ;
  ; Open file if file name is supplied
  ;
  fcbtype = SIZE(file_or_fcb,/type)
  fcbsize = N_ELEMENTS(file_or_fcb)
  IF (fcbsize NE 1) OR ((fcbtype NE 7) AND (fcbtype NE 8)) THEN BEGIN
    message = 'Invalid Filename or FCB supplied'
    GOTO,error_exit
  END
  
  IF fcbtype EQ 7 THEN BEGIN
    fits_open,file_or_fcb,fcb,no_abort=no_abort,message=message
    IF MESSAGE NE '' THEN GOTO,error_exit
  END ELSE fcb = file_or_fcb
  ;
  ; determine which extension to read ==========================================
  ;
  ; case 1: exten_no specified
  ;
  
  enum = exten_no
  IF exten_no LE -1 THEN BEGIN
    ;
    ; case 2: extname, extver, or extlevel specified
    ;
    IF (extname NE '') || (extlevel NE 0) || (extver NE 0) || $
      (xtension NE '') THEN BEGIN
      ;
      ; find extensions with supplied extname, extver, extlevel, and xtension
      ;
      good = REPLICATE(1b,fcb.nextend+1)
      IF extname NE '' THEN good = good AND $
        (STRTRIM(STRUPCASE(extname)) EQ STRUPCASE(fcb.extname))
      IF xtension NE '' THEN good = good AND $
        (STRTRIM(STRUPCASE(xtension)) EQ STRUPCASE(fcb.xtension))
      IF extver NE 0 THEN good = good AND (extver EQ fcb.extver)
      IF extlevel NE 0 THEN good = good AND (extlevel EQ fcb.extlevel)
      good = WHERE(good,ngood)
      ;
      ; select first one
      ;
      IF ngood LE 0 THEN BEGIN
        message='No extension for given extname, extver, and/or' + $
          ' extlevel found'
        GOTO,error_exit
      ENDIF
      enum = good[0]
    END ELSE BEGIN
      ;
      ;       case 3: read next extension
      ;
      enum = fcb.last_extension + 1
      IF (enum EQ 0) && (fcb.naxis[0] EQ 0) THEN enum = 1
    END
  END
  ;
  ; check to see if it is a valid extension
  ;
  IF enum GT fcb.nextend THEN BEGIN
    message='EOF encountered'
    GOTO,error_exit
  END
  ;
  ; extract information from FCB for the extension
  ;
  bitpix = fcb.bitpix[enum]
  naxis = fcb.naxis[enum]
  IF naxis GT 0 THEN axis = fcb.axis[0:naxis-1,enum]
  gcount = fcb.gcount[enum]
  pcount = fcb.pcount[enum]
  xtension = fcb.xtension[enum]
  fcompress = tag_exist(fcb,'fcompress') ? fcb.fcompress : 0
  ;
  ; read header ================================================================
  ;
  IF data_only THEN GOTO,read_data
  h = BYTARR(80,36,/nozero)
  nbytes_in_file = fcb.nbytes
  position = fcb.start_header[enum]
  
  IF fcompress THEN mrd_skip,fcb.unit,position ELSE $
    POINT_LUN,fcb.unit,position
  first_block = 1         ; first block in header flag
  REPEAT BEGIN
    IF position GE nbytes_in_file THEN BEGIN
      message = 'EOF encountered while reading header'
      GOTO,error_exit
    ENDIF
    
    READU,fcb.unit,h
    position +=  2880
    hdr = STRING(h>32b)
    endline = WHERE(STRCMP(hdr,'END     ',8),nend)
    IF nend GT 0 THEN hdr = hdr[0:endline[0]]
    IF first_block THEN header = hdr ELSE header = [header,hdr]
    first_block = 0
  END UNTIL (nend GT 0)
  ;
  ; extract some header information
  ;
  bscale = sxpar(header,'bscale', Count = N_bscale)
  bzero = sxpar(header,'bzero', Count = N_bzero)
  IF bscale EQ 0.0 THEN bscale = 1.0
  unsgn_int = (bitpix EQ 16) && (Bzero EQ 32768) && (bscale EQ 1)
  unsgn_lng = (bitpix EQ 32) && (Bzero EQ 2147483648) && (bscale EQ 1)
  IF (unsgn_int || unsgn_lng) THEN $
    IF ~KEYWORD_SET(no_unsigned) THEN noscale = 1
  IF (N_bscale GT 0) &&(noscale EQ 0) && (data_only EQ 0) && $
    (last EQ 0) && (header_only EQ 0) THEN sxaddpar,header,'bscale',1.0
  IF (N_bzero GT 0) && (noscale EQ 0) && (data_only EQ 0) && $
    (last EQ 0) && (header_only EQ 0) THEN sxaddpar,header,'bzero',0.0
  groups = sxpar(header,'groups')
  ;
  ; create header with form:
  ;       ! Required Keywords
  ;       ! BEGIN MAIN HEADER ------------------------------------------
  ;       ! Primary data unit header keywords
  ;       ! BEGIN EXTENSION HEADER -------------------------------------
  ;       ! Extension header keywords
  ;       ! END
  ;
  ;
  ; add Primary Data Unit header to it portion of the header to it, unless the
  ; NO_PDU keyword is set, or the INHERIT keyword is not found or set to false
  ;
  
  IF no_pdu EQ 0 THEN no_pdu = 1 - (sxpar(header,'INHERIT') > 0)
  IF pdu THEN no_pdu = 0
  IF (no_pdu EQ 0) && (enum GT 0) THEN BEGIN
  
    ;
    ; delete required keywords
    ;
    sxdelpar,header,['SIMPLE','BITPIX','NAXIS','NAXIS1', $
      'NAXIS2','NAXIS3','NAXIS4','NAXIS5', $
      'NAXIS6','NAXIS7','NAXIS8','EXTEND', $
      'PCOUNT','GCOUNT','GROUPS', $
      'XTENSION']
      
      
    ; create required keywords
    ;
    hreq = STRARR(20)
    hreq[0] = 'END     '
    
    IF enum EQ 0 THEN $
      sxaddpar,hreq,'SIMPLE','T','image conforms to FITS standard' $
    ELSE sxaddpar,hreq,'XTENSION',xtension,'extension type'
    
    sxaddpar,hreq,'bitpix',bitpix,'bits per data value'
    sxaddpar,hreq,'naxis',naxis,'number of axes'
    IF naxis GT 0 THEN FOR i=1,naxis DO $
      sxaddpar,hreq,'naxis'+STRTRIM(i,2),axis[i-1]
    IF (enum EQ 0) && (fcb.nextend GE 1) THEN $
      sxaddpar,hreq,'EXTEND','T','file may contain extensions'
    IF groups THEN sxaddpar,hreq,'GROUPS','T','Group format'
    IF (enum GT 0) || (pcount GT 0) THEN $
      sxaddpar,hreq,'PCOUNT',pcount,'Number of group parameters'
    IF (enum GT 0) || (gcount GT 0) THEN $
      sxaddpar,hreq,'GCOUNT',gcount,'Number of groups'
    n0 = WHERE(STRCMP(hreq,'END     ',8)) & n0=n0[0]
    hpdu = fcb.hmain
    n1 = N_ELEMENTS(hpdu)
    IF n1 GT 1 THEN BEGIN
      hreq = [hreq[0:n0-1], $
        'BEGIN MAIN HEADER ---------------------------------', $
        hpdu[0:n1-2], $
        'BEGIN EXTENSION HEADER ----------------------------', $
        'END     ']
      n0 = n0 + n1 + 1
    END
    ;
    ; add extension header
    ;
    header = [hreq[0:n0-1],header]
  END
  IF header_only THEN BEGIN
    data = 0
    GOTO,done
  ENDIF
  ;
  ; Read Data ===================================================================
  ;
  read_data:
  IF naxis EQ 0 THEN BEGIN        ;null image?
    data = 0
    ;
    ; check for implicit data specified by NPIX1, NPIX2, and PIXVALUE (provided
    ; the header was red, i.e. data_only was not specified)
    ;
    IF data_only EQ 0 THEN BEGIN
      NPIX1 = sxpar(header,'NPIX1')
      NPIX2 = sxpar(header,'NPIX2')
      PIXVALUE = sxpar(header,'PIXVALUE')
      IF (NPIX1*NPIX2) GT 0 THEN $
        data = REPLICATE(pixvalue,npix1,npix2)
    END
    GOTO,done
  ENDIF
  
  CASE BITPIX OF
    8:   IDL_type = 1          ; Byte
    16:   IDL_type = 2          ; Integer*2
    32:   IDL_type = 3          ; Integer*4
    -32:   IDL_type = 4          ; Real*4
    -64:   IDL_type = 5          ; Real*8
    ELSE:   BEGIN
      message = 'ERROR - Illegal value of BITPIX (= ' +  $
        STRTRIM(bitpix,2) + ') in FITS header'
      GOTO,error_exit
    END
  ENDCASE
  
  ndata = PRODUCT( axis, /integer )
  bytes_per_word = (ABS(bitpix)/8)
  nbytes_per_group = bytes_per_word * (pcount + ndata)
  nbytes = (gcount>1) * nbytes_per_group
  nwords = nbytes / bytes_per_word
  ;
  ; starting data position
  ;
  
  skip = fcb.start_data[enum] - position
  position = fcb.start_data[enum]
  ;
  ; find correct group
  ;
  IF last EQ 0 THEN BEGIN
    IF group GE (gcount>1) THEN BEGIN
      message='INVALID group number specified'
      GOTO,error_exit
    END
    skip += LONG64(group) * nbytes_per_group
    position += skip
  END
  ;
  ; read group parameters
  ;
  IF (enum EQ 0) && (fcb.random_groups EQ 1) && (pcount GT 0) && $
    (last EQ 0) THEN BEGIN
    IF N_PARAMS() GT 3 THEN BEGIN
      group_par = MAKE_ARRAY( dim = [pcount], type = idl_type, /nozero)
      
      IF fcompress THEN mrd_skip,fcb.unit,skip ELSE $
        POINT_LUN,fcb.unit,position
        
      READU,fcb.unit,group_par
    ENDIF
    skip  =  LONG64(pcount) * bytes_per_word
    position += skip
  ENDIF
  ;
  ; create data array
  ;
  IF last GT 0 THEN BEGIN
    ;
    ; user specified first and last
    ;
    IF (first LT 0) || (last LE 1) || (first GT last) || $
      (last GT nwords-1) THEN BEGIN
      message = 'INVALID value for parameters FIRST & LAST'
      GOTO,error_exit
    ENDIF
    data = MAKE_ARRAY(dim = [last-first+1], type=idl_type, /nozero)
    skip +=  LONG64(first) * bytes_per_word
    position += skip
  ENDIF ELSE BEGIN
    ;
    ; full array
    ;
    IF ndata EQ 0 THEN BEGIN
      data = 0
      GOTO,done
    ENDIF
    IF naxis GT 8 THEN BEGIN
      message = 'Maximum value of NAXIS allowed is 8'
      GOTO,error_exit
    ENDIF
    data = MAKE_ARRAY(dim = axis, type = idl_type, /nozero)
  ENDELSE
  ;
  ; read array
  ;
  IF fcompress THEN mrd_skip,fcb.unit,skip ELSE $
    POINT_LUN,fcb.unit,position
  READU,fcb.unit,data
  ;IF fcb.fcompress THEN swap_endian_inplace,data,/swap_if_little
  IF fcompress THEN swap_endian_inplace,data,/swap_if_little
  IF ~KEYWORD_SET(No_Unsigned) && (~data_only) THEN BEGIN
    IF unsgn_int THEN BEGIN
      data =  UINT(data) - UINT(32768)
    ENDIF ELSE IF unsgn_lng THEN BEGIN
      data = ULONG(data) - ULONG(2147483648)
    ENDIF
  ENDIF
  ;
  ; scale data if header was read and first and last not used.   Do a special
  ; check of an unsigned integer (BZERO = 2^15) or unsigned long (BZERO = 2^31)
  ;
  IF (data_only EQ 0) && (last EQ 0) && (noscale EQ 0) THEN BEGIN
  
    IF bitpix LT 32 THEN BEGIN      ;use real*4 for bitpix<32
      bscale = FLOAT(bscale)
      bzero = FLOAT(bzero)
    ENDIF
    IF bscale NE 1.0 THEN data *= bscale
    IF bzero NE 0.0 THEN data +=  bzero
  ENDIF
  ;
  ; done
  ;
  done:
  IF fcompress THEN BEGIN
    FREE_LUN,fcb.unit
    ff = STRMID(fcb.filename,1,STRLEN(fcb.filename)-2)
    SPAWN,ff,unit=unit,/sh, stderr = stderr
    fcb.unit = unit
  ENDIF ELSE $
    IF fcbtype EQ 7 THEN fits_close,fcb ELSE file_or_fcb.last_extension=enum
  !err = 1
  RETURN
  
  ;
  ; error exit
  ;
  ioerror:
  message = !ERROR_STATE.MSG
  error_exit:
  IF (fcbtype EQ 7) && (N_ELEMENTS(fcb) GT 0) THEN  $
    fits_close,fcb, no_abort=no_abort
  !err = -1
  IF KEYWORD_SET(no_abort) THEN RETURN
  PRINT,'FITS_READ ERROR: '+message
  RETALL
END
