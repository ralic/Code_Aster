subroutine asretm(lmasym, jtmp2, lgtmp2, nbterm, jsmhc,&
                  jsmdi, i1, i2)
    implicit none
    include 'jeveux.h'
    include 'asterfort/jeveut.h'
    include 'asterfort/juveca.h'
    logical :: lmasym
    integer :: jtmp2, lgtmp2, nbterm, jsmhc, jsmdi, i1, i2
    integer :: ideb, ifin, imil
! -----------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! TOLE CRP_4
!     ROUTINE SERVANT A RETENIR OU S'ACCUMULENT LES TERMES ELEMENTAIRES:
!     DANS LE CAS D'UN STOCKAGE MORSE SYMETRIQUE
! -----------------------------------------------------------------
! IN/OUT I JTMP2   : ADRESSE JEVEUX DE L'OBJET ".TMP2"
! IN    I4 JSMHC   : ADRESSE DE ".SMHC".
! IN     I JSMDI   : ADRESSE DE ".SMDI".
! IN     I I1,I2   : NUMEROS GLOBAUX (LIGNE ET COLONNE)
! IN/OUT I NBTERM   : INDICE DU TERME (R/C) A RECOPIER
!                     (ISSU DE LA MATRICE ELEMENTAIRE)
! -----------------------------------------------------------------
    integer :: ili, jco, icoefc, icoefl, i, ncoefc, nubloc
! -----------------------------------------------------------------
    if (i1 .le. i2) then
        ili=i1
        jco=i2
        nubloc=1
    else
        ili=i2
        jco=i1
        nubloc=2
    endif
    if (lmasym) nubloc=1
!
    if (jco .eq. 1) then
        icoefc = 0
    else
        icoefc = zi(jsmdi+jco-2)
    endif
    ncoefc = zi(jsmdi+jco-1) - icoefc
!
!
!     -- CALCUL DE ICOEFL :
!     ------------------------------------------
    icoefl = 0
    if (.false.) then
!     -- RECHERCHE BESTIALE :
        do 10 i = 1, ncoefc
            if (zi4(jsmhc-1+icoefc+i) .eq. ili) then
                icoefl = i
                goto 20
            endif
10      continue
!
    else
!       -- RECHERCHE PAR DICHOTOMIE :
        ideb=1
        ifin=ncoefc
11      continue
        if (ifin-ideb .lt. 5) then
            do 12 i = ideb, ifin
                if (zi4(jsmhc-1+icoefc+i) .eq. ili) then
                    icoefl = i
                    goto 20
                endif
12          continue
        endif
        imil=(ideb+ifin)/2
        if (zi4(jsmhc-1+icoefc+imil) .gt. ili) then
            ifin=imil
        else
            ideb=imil
        endif
        goto 11
    endif
!     IF (ICOEFL.EQ.0 )  CALL U2MESS('F','MODELISA_67')
!
!
20  continue
!
!     -- NBTERM COMPTE LES REELS TRAITES:
    nbterm = nbterm + 1
    if (2*nbterm .gt. lgtmp2) then
        lgtmp2 = 2*lgtmp2
        call juveca('&&ASSMAM.TMP2', lgtmp2)
!         -- IL NE FAUT PAS QUE .TMP2 SOIT LIBERE :
        call jeveut('&&ASSMAM.TMP2', 'E', jtmp2)
    endif
    zi(jtmp2-1+(nbterm-1)*2+1) = nubloc
    zi(jtmp2-1+(nbterm-1)*2+2) = icoefc+icoefl
end subroutine
