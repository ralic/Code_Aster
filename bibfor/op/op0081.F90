subroutine op0081()
    implicit none
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!     BUT:
!       OPERATEUR DE CALCUL DE MACRO-ELEMENT A PARTIR D'UNE BASE MODALE
!       ET DE MATRICES ASSEMBLEES
!
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
!
!      SORTIE :
!-------------
!
! ......................................................................
!
!
!
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/gettco.h"
#include "asterc/r8pi.h"
#include "asterfort/calamo.h"
#include "asterfort/calprc.h"
#include "asterfort/calpro.h"
#include "asterfort/comp81.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/impe81.h"
#include "asterfort/iner81.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedetr.h"
#include "asterfort/refe81.h"
#include "asterfort/remp81.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: ioc, n1, nbval, imod
!
    real(kind=8) :: pi
!
    character(len=8) :: nomres, nomcon, nomope, mailla, basmod, blanc
    character(len=19) :: raid, mass, amor, impe, typmat
    character(len=24) :: nommat
    integer :: nbmod, iocm, iocf, ioca, vali(2)
    integer :: lmass, lrigi, lamor, ldref, ldres
!
    data blanc /'        '/
!
!-----------------------------------------------------------------------
!
    call infmaj()
    call getres(nomres, nomcon, nomope)
!
    pi=r8pi()
!
! --- MACR_ELEM_DYNA OBTENU PAR MODELE NUMERIQUE OU EXPERIMENTAL
    call getfac('MODELE_MESURE', nbval)
!
! --- RECUPERATION BASE MODALE, MATRICES ET CREATION .REFE
!     ET DETERMINATION OPTION DE CALCUL
!
    call refe81(nomres, basmod, raid, mass, amor,&
                mailla)
!
! --- CALCUL DES MATRICES PROJETEES (SI PAS DE MOT-CLE MODELE_MESURE)
!
    if (nbval .eq. 0) then
        impe = blanc
        call getvid(' ', 'MATR_IMPE', scal=impe, nbret=n1)
        if (impe .ne. blanc) then
            call impe81(nomres, impe, basmod)
            typmat='MATR_ASSE_DEPL_R'
            goto 10
        endif
        call getvid(' ', 'MATR_IMPE_RIGI', scal=impe, nbret=n1)
        if (impe .ne. blanc) then
            call impe81(nomres, impe, basmod)
            typmat='MATR_ASSE_DEPL_R'
            goto 10
        endif
        call getvid(' ', 'MATR_IMPE_MASS', scal=impe, nbret=n1)
        if (impe .ne. blanc) then
            call impe81(nomres, impe, basmod)
            typmat='MATR_ASSE_DEPL_R'
            goto 10
        endif
        call getvid(' ', 'MATR_IMPE_AMOR', scal=impe, nbret=n1)
        if (impe .ne. blanc) then
            call impe81(nomres, impe, basmod)
            typmat='MATR_ASSE_DEPL_R'
            goto 10
        endif
!
        nommat = nomres//'.MAEL_RAID'
        call gettco(raid, typmat)
!
        if (typmat .eq. 'MATR_ASSE_DEPL_R') then
            call calpro(nommat, 'G', basmod, raid)
        else if (typmat.eq.'MATR_ASSE_DEPL_C') then
            call calprc(nommat, 'G', basmod, raid)
        else
            call utmess('F', 'ALGORITH14_17', sk=typmat)
        endif
!
        nommat = nomres//'.MAEL_MASS'
        call calpro(nommat, 'G', basmod, mass)
!
        if (amor .ne. blanc) then
            nommat=nomres//'.MAEL_AMOR'
            call calpro(nommat, 'G', basmod, amor)
        else
            call getvr8(blanc, 'AMOR_REDUIT', iocc=1, nbval=0, nbret=ioc)
            if (ioc .lt. 0) then
                nommat=nomres//'.MAEL_AMOR'
                call calamo(nommat, 'G', basmod)
            endif
        endif
!
! ---   CALCUL DES FORCES D'INERTIES
!
        nommat = nomres//'.MAEL_INER'
        call iner81(nommat, 'G', basmod, mass)
 10     continue
!
! ------------------------------------------------------------------- C
!
    else
!
! -----  CAS OU LES MATRICES SONT REMPLIES A LA MAIN
!
        call dismoi('NB_MODES_TOT', basmod, 'RESULTAT', repi=nbmod)
!
! ---    RECUPERATION DES VALEURS DE MASSES GENERALISEES ET VERIF :
! ---    LE NOMBRE DE VALEURS ENTREES = NOMBRE DE VECT DE LA BASE
!
! ---   MASSE GENERALISEE
        call wkvect('&&OP0081.MASS', 'V V R', nbmod, lmass)
        call getvr8('MODELE_MESURE', 'MASS_GENE', iocc=1, nbval=nbmod, vect=zr( lmass),&
                    nbret=iocm)
        if (iocm .ne. nbmod) then
            vali(1) = nbmod
            vali(2) = iocm
            if (iocm .lt. 0) vali(2) = -iocm
            call utmess('F', 'ALGORITH17_31', sk='MASSE_GENE', ni=2, vali=vali)
        endif
!
! ---   FREQUENCES PROPRES
        call wkvect('&&OP0081.RIGI', 'V V R', nbmod, lrigi)
        call getvr8('MODELE_MESURE ', 'FREQ', iocc=1, nbval=nbmod, vect=zr(lrigi),&
                    nbret=iocf)
!
        if (iocf .ne. nbmod) then
            vali(1) = nbmod
            vali(2) = iocf
            if (iocf .lt. 0) vali(2) = -iocm
            call utmess('F', 'ALGORITH17_31', sk='FREQ', ni=2, vali=vali)
        endif
!
! ---   AMORTISSEMENTS REDUITS
        call wkvect('&&OP0081.AMOR', 'V V R', nbmod, lamor)
        call getvr8('MODELE_MESURE', 'AMOR_REDUIT', iocc=1, nbval=nbmod, vect=zr( lamor),&
                    nbret=ioca)
        if (ioca .ne. 0 .and. ioca .ne. nbmod) then
            vali(1) = nbmod
            vali(2) = ioca
            if (iocf .lt. 0) vali(2) = -iocm
            call utmess('F', 'ALGORITH17_31', sk='FREQ', ni=2, vali=vali)
        endif
!
! ----- REMPLISSAGE
!
! ---   MASS_GENE : DIRECT
        call remp81(nomres//'.MAEL_MASS', lmass, basmod, nbmod)
!
! ---   RIGI_GENE : LES CALCULER A PARTIR DE MASSE_GENE ET FREQ
        do imod = 1, nbmod
            zr(lrigi-1+imod) = 4*pi**2*zr(lrigi-1+imod)**2 *zr(lmass- 1+imod)
        end do
        call remp81(nomres//'.MAEL_RAID', lrigi, basmod, nbmod)
!
! ---   AMOR_GENE : LES CALCULER APARTIR D'AMOR_REDUIT, MASSE ET RIGI
        if (ioca .ne. 0) then
            do imod = 1, nbmod
                zr(lamor-1+imod) = 2*zr(lamor-1+imod)*sqrt(zr(lrigi-1+ imod)* zr(lmass-1+imod))
            end do
            call remp81(nomres//'.MAEL_AMOR', lamor, basmod, nbmod)
        endif
!
! ---   REMPLLISSAGE DES FORCES D'INERTIES
!
        call wkvect(nomres//'.MAEL_INER_REFE', 'G V K24', 2, ldref)
        zk24(ldref)=basmod
        zk24(ldref+1)='        '
        call wkvect(nomres//'.MAEL_INER_VALE', 'G V R', 3*nbmod, ldres)
!
! ---   LE VEC DES MASSES EFFE EST REMPLI A 0, ON EMET UNE ALARME
!       (ON NE PEUT PAS LE REMPLIR, ON NE CONNAIT PAS LA MAT DE MASSE)
        call utmess('A', 'ALGORITH17_32', sk=' ')
        do imod = 1, 3*nbmod
            zr(ldres-1+imod) = 0.d0
        end do
!
!
    endif
!
! --- COMPATIBILITE AVEC SD MACR_ELEM_STAT
!
    call comp81(nomres, basmod, raid, mailla)
!
! --- MENAGE
    call jedetr('&&OP0081.MASS')
    call jedetr('&&OP0081.RIGI')
    call jedetr('&&OP0081.AMOR')
!
end subroutine
