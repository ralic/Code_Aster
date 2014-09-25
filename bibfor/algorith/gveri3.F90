subroutine gveri3(chfond, taillr, config, lnoff, thlagr,&
                  thlag2, ndeg, trav1, trav2, trav3,&
                  typdis)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    implicit none
!
!     ------------------------------------------------------------------
!
! FONCTION REALISEE:     DANS LE CADRE DE X-FEM et FEM
!
!     - METHODES THETA_LAGRANGE,THETA_LAGRANGE_REGU
!
!         POUR CHAQUE NOEUD DU FOND DE FISSURE GAMM0 ON RECUPERE
!         LE DOUBLET (RINF, RSUP )
!
!     - METHODE THETA_LEGENDRE
!
!         POUR CHAQUE NOEUD DU FOND DE FISSURE GAMM0 ON RECUPERE
!         LE TRIPLET ( DEGRE DES POLYNOMES DE LEGENDRE, RINF, RSUP )
!
!     ------------------------------------------------------------------
! ENTREE:
!        CHFOND : NOMS DES NOEUDS
!        TAILLR : TAILLES DE MAILLES CONNECTEES AUX NOEUDS
!        CONFIG : CONFIGURATION DE LA FISSURE EN FEM
!        LNOFF  : NOMBRE DE NOEUD DE GAMM0
!        THLAGR : SI THETA_LAGRANGE  THLAGR = .TRUE.
!        THLAG2 : SI THETA_LAGRANGE_REGU  THLAG2 = .TRUE.
!        NDEG   : DEGRE DES POLYNOMES DE LEGENDRE
!
! SORTIE:
!        RINF          ( OBJET TRAV1 )
!        RSUP          ( OBJET TRAV2 )
!        MODULE(THETA) ( OBJET TRAV3 )
!     ------------------------------------------------------------------
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/fointe.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/glegen.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=16) :: typdis
    character(len=24) :: trav0, trav1, trav2, trav3, chfond, absgam, taillr
    character(len=8) :: config, nompar(1), rinff, rsupf
!
    integer :: lnoff, ndeg, nbre, nr, nrf, nbpar, i, j
    integer :: iadrt0, iadrt1, iadrt2, iadrt3, ifon, iadabs, ier
    integer :: iatmno
!
    real(kind=8) :: maxtai, mintai, rinf, rsup, xl, valpar(1), valres
    real(kind=8) :: valr(2)
!
    aster_logical :: thlagr, thlag2
!
!
    call jemarq()
!
    if (thlag2) then
        nbre = 1+lnoff/2 - 1
    else if (thlagr) then
        nbre = lnoff - 1
    else
        nbre = ndeg
    endif
!
! ALLOCATION DE 3 OBJETS DE TRAVAIL
!
    if(typdis.ne.'COHESIF') then
        trav0 = '&&VERIFG.GAM0'//'           '
        trav1 = '&&VERIFG.RINF'//'           '
        trav2 = '&&VERIFG.RSUP'//'           '
        call wkvect(trav0, 'V V K8', lnoff, iadrt0)
        call wkvect(trav1, 'V V R', lnoff, iadrt1)
        call wkvect(trav2, 'V V R', lnoff, iadrt2)
    endif
    trav3 = '&&VERIFG.THET'//'           '
    call wkvect(trav3, 'V V R', (nbre+1)*lnoff, iadrt3)
    if(typdis.eq.'COHESIF') goto 98
!
    call getvr8('THETA', 'R_INF', iocc=1, scal=rinf, nbret=nr)
    call getvr8('THETA', 'R_SUP', iocc=1, scal=rsup, nbret=nr)
    if (nr .ne. 0 .and. rsup .le. rinf) then
        call utmess('F', 'RUPTURE1_6')
    endif
    call getvid('THETA', 'R_INF_FO', iocc=1, scal=rinff, nbret=nrf)
    call getvid('THETA', 'R_SUP_FO', iocc=1, scal=rsupf, nbret=nrf)
!     RECUPERATION DE RINF ET DE RSUP DANS LA SD FOND_FISS
    if (nr .eq. 0 .and. nrf .eq. 0) then
        if (config .eq. 'DECOLLEE') then
            call utmess('F', 'RUPTURE1_7')
        endif
        call jeveuo(taillr, 'L', iatmno)
        maxtai = 0.d0
        mintai = zr(iatmno)
        do 1 j = 1, lnoff
            maxtai = max(maxtai,zr(iatmno-1+j))
            mintai = min(mintai,zr(iatmno-1+j))
  1     continue
        rinf = 2*maxtai
        rsup = 4*maxtai
        valr(1) = rinf
        valr(2) = rsup
        call utmess('I', 'RUPTURE1_5', nr=2, valr=valr)
        valr(1) = mintai
        valr(2) = maxtai
        if (maxtai .gt. 2*mintai) then
            call utmess('A', 'RUPTURE1_16', nr=2, valr=valr)
        endif
    endif
!
    call jeveuo(chfond, 'L', ifon)
    absgam='&&GVERI3.TEMP     .ABSCU'
    call wkvect(absgam, 'V V R', lnoff, iadabs)
    do 10 i = 1, lnoff
        zr(iadabs-1+(i-1)+1)=zr(ifon-1+4*(i-1)+4)
 10 end do
    xl=zr(iadabs-1+(lnoff-1)+1)
!
    if (.not.thlagr .and. .not.thlag2) then
!
! METHODE THETA_LEGENDRE
!
        do 50 j = 1, lnoff
            zk8(iadrt0 + j - 1) = 'PTFONFIS'
            if (nrf .ne. 0) then
                nbpar = 1
                nompar(1) = 'ABSC'
                valpar(1) = zr(iadabs + j - 1)
                call fointe('FM', rinff, nbpar, nompar, valpar,&
                            valres, ier)
                zr(iadrt1 + j - 1) = valres
                call fointe('FM', rsupf, nbpar, nompar, valpar,&
                            valres, ier)
                zr(iadrt2 + j - 1) = valres
                if (zr(iadrt2 + j - 1) .le. zr(iadrt1 + j - 1)) then
                    call utmess('F', 'RUPTURE1_6')
                endif
            else
                zr(iadrt1 + j - 1) = rinf
                zr(iadrt2 + j - 1) = rsup
            endif
 50     continue
!
        call glegen(nbre, lnoff, xl, absgam, zr(iadrt3))
!
    else if (thlagr.or.thlag2) then
!
! METHODES THETA_LAGRANGE,THETA_LAGRANGE_REGU
!
        do 60 j = 1, lnoff
            zk8(iadrt0 + j - 1) = 'PTFONFIS'
            if (nrf .ne. 0) then
                nbpar = 1
                nompar(1) = 'ABSC'
                valpar(1) = zr(iadabs + j - 1)
                call fointe('FM', rinff, nbpar, nompar, valpar,&
                            valres, ier)
                zr(iadrt1 + j - 1) = valres
                call fointe('FM', rsupf, nbpar, nompar, valpar,&
                            valres, ier)
                zr(iadrt2 + j - 1) = valres
                if (zr(iadrt2 + j - 1) .le. zr(iadrt1 + j - 1)) then
                    call utmess('F', 'RUPTURE1_6')
                endif
            else
                zr(iadrt1 + j - 1) = rinf
                zr(iadrt2 + j - 1) = rsup
            endif
 60     continue
!
    endif
!
    call jedetr(absgam)
    call jedetr(trav0)
98  continue
!
    call jedema()
end subroutine
