subroutine coecis(napcis, foncis)
    implicit none
#include "jeveux.h"
!
#include "asterfort/codent.h"
#include "asterfort/foston.h"
#include "asterfort/jecrec.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
    character(len=19) :: napcis, foncis
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!       NAPPE DES COEFFICIENTS DE CISAILLEMENT POUR SECTION='RECTANGLE'
!       FONCTION DES COEFFICIENTS DE CISAILLEMENT POUR SECTION='CERCLE'
!     ------------------------------------------------------------------
!
    integer :: nbvale, nbfonc, nprol, nbvalf
    integer :: lpro, lpar, jval, lnomf, ladrf, lval, i, ifonc, ival
    integer :: nbvfon, lpro2, jval2, lval2, ival2, lfon
!     ------------------------------------------------------------------
!
    call jemarq()
!
!     ##################################################################
!     ######DEFINITION DE LA NAPPE DES COEFFICIENTS DE CISAILLEMENT#####
!     ####################SECTION = 'RECTANGLE'#########################
!     ##################################################################
    napcis = 'NAPPE_'
!
!     NOMBRE DE FONCTIONS
    nbfonc = 12
!
!     NOMBRE DE VALEURS PAR FONCTIONS
    nbvalf = 12
!
    nprol = 7 + 2*nbfonc
    call wkvect(napcis//'.PROL', 'V V K24', nprol, lpro)
    zk24(lpro ) = 'NAPPE   '
    zk24(lpro+1) = 'LIN LIN '
    zk24(lpro+2) = 'ALPHA'
    zk24(lpro+3) = 'A'
    zk24(lpro+4) = 'EE'
    zk24(lpro+5) = napcis
    zk24(lpro+6) = 'BETA'
    do 10 i = 0, nbfonc-1
        zk24(lpro+7+i*2) = 'LIN LIN '
        zk24(lpro+7+i*2+1) = 'EE'
10  end do
!
    call wkvect(napcis//'.PARA', 'V V R', nbfonc, lpar)
!     VALEURS DE ALPHA
    zr(lpar) = 0.d0
    zr(lpar+1) = 0.05d0
    zr(lpar+2) = 0.1d0
    zr(lpar+3) = 0.2d0
    zr(lpar+4) = 0.3d0
    zr(lpar+5) = 0.4d0
    zr(lpar+6) = 0.5d0
    zr(lpar+7) = 0.6d0
    zr(lpar+8) = 0.7d0
    zr(lpar+9) = 0.8d0
    zr(lpar+10) = 0.9d0
    zr(lpar+11) = 0.95d0
!
!     NOMBRE DE VALEURS TOTAL PAR FONCTION
    nbvale = nbfonc*2
    call wkvect('&&COECIS.VALEURS', 'V V R', nbvale, jval)
!
    call wkvect('&&COECIS.NOM.FONCTIONS', 'V V K24', nbfonc, lnomf)
    call wkvect('&&COECIS.POINTEURS.F', 'V V I', nbfonc, ladrf)
    do 30 ifonc = 1, nbfonc
        zk24(lnomf+ifonc-1) = '&&COECIS.F'
        call codent(ifonc, 'G', zk24(lnomf+ifonc-1)(11:19))
        zk24(lnomf+ifonc-1)(20:24) = '.VALE'
!        VALEURS DES FONCTIONS A(BETA)
        if (ifonc .eq. 1) then
            zr(jval) = 0.d0
            zr(jval+1) = 1.2d0
            zr(jval+2) = 0.05d0
            zr(jval+3) = 1.2d0
            zr(jval+4) = 0.1d0
            zr(jval+5) = 1.2d0
            zr(jval+6) = 0.2d0
            zr(jval+7) = 1.2d0
            zr(jval+8) = 0.3d0
            zr(jval+9) = 1.2d0
            zr(jval+10) = 0.4d0
            zr(jval+11) = 1.2d0
            zr(jval+12) = 0.5d0
            zr(jval+13) = 1.2d0
            zr(jval+14) = 0.6d0
            zr(jval+15) = 1.2d0
            zr(jval+16) = 0.7d0
            zr(jval+17) = 1.2d0
            zr(jval+18) = 0.8d0
            zr(jval+19) = 1.2d0
            zr(jval+20) = 0.9d0
            zr(jval+21) = 1.2d0
            zr(jval+22) = 0.95d0
            zr(jval+23) = 1.2d0
        else if (ifonc.eq.2) then
            zr(jval) = 0.d0
            zr(jval+1) = 1.2d0
            zr(jval+2) = 0.05d0
            zr(jval+3) = 1.2091d0
            zr(jval+4) = 0.1d0
            zr(jval+5) = 1.22903d0
            zr(jval+6) = 0.2d0
            zr(jval+7) = 1.29961d0
            zr(jval+8) = 0.3d0
            zr(jval+9) = 1.41338d0
            zr(jval+10) = 0.4d0
            zr(jval+11) = 1.57689d0
            zr(jval+12) = 0.5d0
            zr(jval+13) = 1.80293d0
            zr(jval+14) = 0.6d0
            zr(jval+15) = 2.11482d0
            zr(jval+16) = 0.7d0
            zr(jval+17) = 2.56072d0
            zr(jval+18) = 0.8d0
            zr(jval+19) = 3.26505d0
            zr(jval+20) = 0.9d0
            zr(jval+21) = 4.71451d0
            zr(jval+22) = 0.95d0
            zr(jval+23) = 6.68937d0
        else if (ifonc.eq.3) then
            zr(jval) = 0.d0
            zr(jval+1) = 1.2d0
            zr(jval+2) = 0.05d0
            zr(jval+3) = 1.21234d0
            zr(jval+4) = 0.1d0
            zr(jval+5) = 1.23638d0
            zr(jval+6) = 0.2d0
            zr(jval+7) = 1.31674d0
            zr(jval+8) = 0.3d0
            zr(jval+9) = 1.44242d0
            zr(jval+10) = 0.4d0
            zr(jval+11) = 1.62077d0
            zr(jval+12) = 0.5d0
            zr(jval+13) = 1.86614d0
            zr(jval+14) = 0.6d0
            zr(jval+15) = 2.20702d0
            zr(jval+16) = 0.7d0
            zr(jval+17) = 2.70395d0
            zr(jval+18) = 0.8d0
            zr(jval+19) = 3.51953d0
            zr(jval+20) = 0.9d0
            zr(jval+21) = 5.35808d0
            zr(jval+22) = 0.95d0
            zr(jval+23) = 8.19368d0
        else if (ifonc.eq.4) then
            zr(jval) = 0.d0
            zr(jval+1) = 1.2d0
            zr(jval+2) = 0.05d0
            zr(jval+3) = 1.21725d0
            zr(jval+4) = 0.1d0
            zr(jval+5) = 1.24671d0
            zr(jval+6) = 0.2d0
            zr(jval+7) = 1.3388d0
            zr(jval+8) = 0.3d0
            zr(jval+9) = 1.47742d0
            zr(jval+10) = 0.4d0
            zr(jval+11) = 1.67093d0
            zr(jval+12) = 0.5d0
            zr(jval+13) = 1.93642d0
            zr(jval+14) = 0.6d0
            zr(jval+15) = 2.30888d0
            zr(jval+16) = 0.7d0
            zr(jval+17) = 2.8664d0
            zr(jval+18) = 0.8d0
            zr(jval+19) = 3.82993d0
            zr(jval+20) = 0.9d0
            zr(jval+21) = 6.21611d0
            zr(jval+22) = 0.95d0
            zr(jval+23) = 10.2937d0
        else if (ifonc.eq.5) then
            zr(jval) = 0.d0
            zr(jval+1) = 1.2d0
            zr(jval+2) = 0.05d0
            zr(jval+3) = 1.22028d0
            zr(jval+4) = 0.1d0
            zr(jval+5) = 1.25225d0
            zr(jval+6) = 0.2d0
            zr(jval+7) = 1.34797d0
            zr(jval+8) = 0.3d0
            zr(jval+9) = 1.48853d0
            zr(jval+10) = 0.4d0
            zr(jval+11) = 1.6827d0
            zr(jval+12) = 0.5d0
            zr(jval+13) = 1.9487d0
            zr(jval+14) = 0.6d0
            zr(jval+15) = 2.32387d0
            zr(jval+16) = 0.7d0
            zr(jval+17) = 2.89403d0
            zr(jval+18) = 0.8d0
            zr(jval+19) = 3.90732d0
            zr(jval+20) = 0.9d0
            zr(jval+21) = 6.53601d0
            zr(jval+22) = 0.95d0
            zr(jval+23) = 11.2361d0
        else if (ifonc.eq.6) then
            zr(jval) = 0.d0
            zr(jval+1) = 1.2d0
            zr(jval+2) = 0.05d0
            zr(jval+3) = 1.22125d0
            zr(jval+4) = 0.1d0
            zr(jval+5) = 1.25299d0
            zr(jval+6) = 0.2d0
            zr(jval+7) = 1.34528d0
            zr(jval+8) = 0.3d0
            zr(jval+9) = 1.47872d0
            zr(jval+10) = 0.4d0
            zr(jval+11) = 1.66196d0
            zr(jval+12) = 0.5d0
            zr(jval+13) = 1.91269d0
            zr(jval+14) = 0.6d0
            zr(jval+15) = 2.26723d0
            zr(jval+16) = 0.7d0
            zr(jval+17) = 2.81021d0
            zr(jval+18) = 0.8d0
            zr(jval+19) = 3.79004d0
            zr(jval+20) = 0.9d0
            zr(jval+21) = 6.40058d0
            zr(jval+22) = 0.95d0
            zr(jval+23) = 11.1893d0
        else if (ifonc.eq.7) then
            zr(jval) = 0.d0
            zr(jval+1) = 1.2d0
            zr(jval+2) = 0.05d0
            zr(jval+3) = 1.22011d0
            zr(jval+4) = 0.1d0
            zr(jval+5) = 1.24905d0
            zr(jval+6) = 0.2d0
            zr(jval+7) = 1.33181d0
            zr(jval+8) = 0.3d0
            zr(jval+9) = 1.45075d0
            zr(jval+10) = 0.4d0
            zr(jval+11) = 1.61403d0
            zr(jval+12) = 0.5d0
            zr(jval+13) = 1.83775d0
            zr(jval+14) = 0.6d0
            zr(jval+15) = 2.1543d0
            zr(jval+16) = 0.7d0
            zr(jval+17) = 2.64029d0
            zr(jval+18) = 0.8d0
            zr(jval+19) = 3.52423d0
            zr(jval+20) = 0.9d0
            zr(jval+21) = 5.91643d0
            zr(jval+22) = 0.95d0
            zr(jval+23) = 10.3749d0
        else if (ifonc.eq.8) then
            zr(jval) = 0.d0
            zr(jval+1) = 1.2d0
            zr(jval+2) = 0.05d0
            zr(jval+3) = 1.21699d0
            zr(jval+4) = 0.1d0
            zr(jval+5) = 1.24091d0
            zr(jval+6) = 0.2d0
            zr(jval+7) = 1.30911d0
            zr(jval+8) = 0.3d0
            zr(jval+9) = 1.40786d0
            zr(jval+10) = 0.4d0
            zr(jval+11) = 1.54462d0
            zr(jval+12) = 0.5d0
            zr(jval+13) = 1.73307d0
            zr(jval+14) = 0.6d0
            zr(jval+15) = 2.00002d0
            zr(jval+16) = 0.7d0
            zr(jval+17) = 2.40936d0
            zr(jval+18) = 0.8d0
            zr(jval+19) = 3.15426d0
            zr(jval+20) = 0.9d0
            zr(jval+21) = 5.18602d0
            zr(jval+22) = 0.95d0
            zr(jval+23) = 9.01404d0
        else if (ifonc.eq.9) then
            zr(jval) = 0.d0
            zr(jval+1) = 1.2d0
            zr(jval+2) = 0.05d0
            zr(jval+3) = 1.21234d0
            zr(jval+4) = 0.1d0
            zr(jval+5) = 1.22963d0
            zr(jval+6) = 0.2d0
            zr(jval+7) = 1.27969d0
            zr(jval+8) = 0.3d0
            zr(jval+9) = 1.35418d0
            zr(jval+10) = 0.4d0
            zr(jval+11) = 1.45982d0
            zr(jval+12) = 0.5d0
            zr(jval+13) = 1.60759d0
            zr(jval+14) = 0.6d0
            zr(jval+15) = 1.81808d0
            zr(jval+16) = 0.7d0
            zr(jval+17) = 2.13953d0
            zr(jval+18) = 0.8d0
            zr(jval+19) = 2.71977d0
            zr(jval+20) = 0.9d0
            zr(jval+21) = 4.29975d0
            zr(jval+22) = 0.95d0
            zr(jval+23) = 7.29574d0
        else if (ifonc.eq.10) then
            zr(jval) = 0.d0
            zr(jval+1) = 1.2d0
            zr(jval+2) = 0.05d0
            zr(jval+3) = 1.20699d0
            zr(jval+4) = 0.1d0
            zr(jval+5) = 1.21695d0
            zr(jval+6) = 0.2d0
            zr(jval+7) = 1.24713d0
            zr(jval+8) = 0.3d0
            zr(jval+9) = 1.29483d0
            zr(jval+10) = 0.4d0
            zr(jval+11) = 1.36597d0
            zr(jval+12) = 0.5d0
            zr(jval+13) = 1.46914d0
            zr(jval+14) = 0.6d0
            zr(jval+15) = 1.61905d0
            zr(jval+16) = 0.7d0
            zr(jval+17) = 1.84773d0
            zr(jval+18) = 0.8d0
            zr(jval+19) = 2.25238d0
            zr(jval+20) = 0.9d0
            zr(jval+21) = 3.33064d0
            zr(jval+22) = 0.95d0
            zr(jval+23) = 5.37206d0
        else if (ifonc.eq.11) then
            zr(jval) = 0.d0
            zr(jval+1) = 1.2d0
            zr(jval+2) = 0.05d0
            zr(jval+3) = 1.20223d0
            zr(jval+4) = 0.1d0
            zr(jval+5) = 1.20561d0
            zr(jval+6) = 0.2d0
            zr(jval+7) = 1.21707d0
            zr(jval+8) = 0.3d0
            zr(jval+9) = 1.23759d0
            zr(jval+10) = 0.4d0
            zr(jval+11) = 1.27159d0
            zr(jval+12) = 0.5d0
            zr(jval+13) = 1.3254d0
            zr(jval+14) = 0.6d0
            zr(jval+15) = 1.40907d0
            zr(jval+16) = 0.7d0
            zr(jval+17) = 1.54135d0
            zr(jval+18) = 0.8d0
            zr(jval+19) = 1.77105d0
            zr(jval+20) = 0.9d0
            zr(jval+21) = 2.33808d0
            zr(jval+22) = 0.95d0
            zr(jval+23) = 3.36741d0
        else if (ifonc.eq.12) then
            zr(jval) = 0.d0
            zr(jval+1) = 1.2d0
            zr(jval+2) = 0.05d0
            zr(jval+3) = 1.20064d0
            zr(jval+4) = 0.1d0
            zr(jval+5) = 1.20171d0
            zr(jval+6) = 0.2d0
            zr(jval+7) = 1.20582d0
            zr(jval+8) = 0.3d0
            zr(jval+9) = 1.2142d0
            zr(jval+10) = 0.4d0
            zr(jval+11) = 1.22956d0
            zr(jval+12) = 0.5d0
            zr(jval+13) = 1.25619d0
            zr(jval+14) = 0.6d0
            zr(jval+15) = 1.30111d0
            zr(jval+16) = 0.7d0
            zr(jval+17) = 1.37761d0
            zr(jval+18) = 0.8d0
            zr(jval+19) = 1.51661d0
            zr(jval+20) = 0.9d0
            zr(jval+21) = 1.84134d0
            zr(jval+22) = 0.95d0
            zr(jval+23) = 2.37116d0
        endif
        call wkvect(zk24(lnomf+ifonc-1), 'V V R', nbvalf*2, lval)
        zi(ladrf+ifonc-1) = lval
        do 32 ival = 1, nbvalf
            zr(lval-1+ival) = zr(jval-1+2*ival-1)
            zr(lval-1+nbvalf+ival) = zr(jval-1+2*ival)
32      continue
        zk24(lpro+6+2*ifonc)(1:1) = zk24(lpro+4)(1:1)
        zk24(lpro+6+2*ifonc)(2:2) = zk24(lpro+6+2*ifonc)(1:1)
30  end do
!
    call jecrec(napcis//'.VALE', 'V V R', 'NU', 'CONTIG', 'VARIABLE',&
                nbfonc)
    call foston(napcis//'.VALE', zk24(lnomf), nbfonc)
!
!     ##################################################################
!     ####DEFINITION DE LA FONCTION DES COEFFICIENTS DE CISAILLEMENT####
!     ######################SECTION = 'CERCLE'##########################
!     ##################################################################
!
    foncis = 'FONCTION'
!
!     NOMBRE DE VALEURS POUR LA FONCTION
    nbvfon = 14
!
    call wkvect(foncis//'.PROL', 'V V K24', 6, lpro2)
    zk24(lpro2) = 'FONCTION'
    zk24(lpro2+1) = 'LIN LIN '
    zk24(lpro2+2) = 'ALPHA'
    zk24(lpro2+3) = 'CCIS'
    zk24(lpro2+4) = 'EE'
    zk24(lpro2+5) = foncis
!
    call wkvect('&&COECIS.VALEURS_FON', 'V V R', nbvfon*2, jval2)
!     VALEURS DE LA FONCTION
    zr(jval2) = 0.d0
    zr(jval2+1) = 1.16667d0
    zr(jval2+2) = 0.05d0
    zr(jval2+3) = 1.17442d0
    zr(jval2+4) = 0.1d0
    zr(jval2+5) = 1.19877d0
    zr(jval2+6) = 0.2d0
    zr(jval2+7) = 1.28947d0
    zr(jval2+8) = 0.3d0
    zr(jval2+9) = 1.41882d0
    zr(jval2+10) = 0.4d0
    zr(jval2+11) = 1.56277d0
    zr(jval2+12) = 0.5d0
    zr(jval2+13) = 1.69984d0
    zr(jval2+14) = 0.6d0
    zr(jval2+15) = 1.81536d0
    zr(jval2+16) = 0.7d0
    zr(jval2+17) = 1.90233d0
    zr(jval2+18) = 0.8d0
    zr(jval2+19) = 1.95983d0
    zr(jval2+20) = 0.9d0
    zr(jval2+21) = 1.99081d0
    zr(jval2+22) = 0.95d0
    zr(jval2+23) = 1.99781d0
    zr(jval2+24) = 0.99d0
    zr(jval2+25) = 1.99992d0
    zr(jval2+26) = 1.d0
    zr(jval2+27) = 2.d0
    call wkvect(foncis//'.VALE', 'V V R', nbvfon*2, lval2)
    lfon = lval2 + nbvfon
    do 20 ival2 = 0, nbvfon-1
        zr(lval2+ival2) = zr(jval2+2*ival2)
        zr(lfon+ival2) = zr(jval2+2*ival2+1)
20  end do
!
    call jedema()
end subroutine
