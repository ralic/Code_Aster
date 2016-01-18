subroutine gverig(nocc, chfond, taillr, config,&
                  lobj2, nomno, coorn, trav1, trav2,&
                  trav3, trav4)
    implicit none
!     ------------------------------------------------------------------
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
! FONCTION REALISEE:
!
!     MOTS CLE FACTEUR THETA:
!
!     POUR CHAQUE NOEUD DU FOND DE FISSURE GAMM0 ON RECUPERE
!     LE TRIPLET ( MODULE(THETA), R_INF, R_SUP )
!
!     PUIS ON VERIFIE:
!                     QUE LES NOMS DE GROUPE OU D'ELEMENTS (NOEUD)
!                     APPARTIENNENT BIEN AU MAILLAGE ET A GAMMA0
!
!                     QU'IL N'Y A PAS DUPLICATION DES ENTITES
!
!                     QUE GAMM0 EST COMPLET
!
!                  ---------------------------------
!
!
!     ------------------------------------------------------------------
! ENTREE:
!        NOMA   : NOM DU MAILLAGE
!        NOCC   : NOMBRE D'OCCURENCES
!        NOMNO  : NOM DE L'OBJET CONTENANT LES NOMS DES NOEUDS
!        CHFOND : NOMS DES NOEUDS
!        TAILLR : TAILLES DE MAILLES CONNECTEES AUX NOEUDS
!        CONFIG : CONFIGURATION DE LA FISSURE
!        COORN  : NOM DE L'OBJET CONTENANT LES COORDONNEES DES NOEUDS
!        LOBJ2  : NOMBRE DE NOEUDS DE GAMM0
!
! SORTIE:
!        R_INF         ( OBJET TRAV1 )
!        R_SUP         ( OBJET TRAV2 )
!        MODULE(THETA) ( OBJET TRAV3 )
!        ABSC_CURV     ( OBJET TRAV4 )
!     ------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterfort/fointe.h"
#include "asterfort/gabscu.h"
#include "asterfort/getvem.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/lxlgut.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=24) :: chfond, nomno, coorn, taillr
    character(len=24) :: trav, trav0, trav1, trav2, trav3, trav4, trav5
    character(len=8) :: config, noeud
    character(len=8) :: nompar(1), rinff, rsupf, thetf
    character(len=16) :: motfac
!
    integer :: jjj, iocc, nocc, ndim, lobj2, nbmof
    integer :: nbpar, ito, nto, jjj2, l2, l
    integer :: nbm, nbmf, iadrno, iatmno, iadrco, iadrt0, iadrt1
    integer :: nbre, iadrt2, iadrt3, iadabs, i, ier, j, num
!
    real(kind=8) :: maxtai, mintai, rinf, rsup, thet, xl, valpar(1), valres
!
    real(kind=8) :: valr(2)
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
!
    call jeveuo(chfond, 'L', iadrno)
    call jeveuo(coorn, 'L', iadrco)
!
    motfac = 'THETA'
    
    l = len(motfac)
    l2 = lxlgut(motfac)
    nbre = 0
!
! ALLOCATION DE 3 OBJETS DE TRAVAIL
!
    trav0 = '&&VERIFG.GAM0'//'           '
    trav1 = '&&VERIFG.RINF'//'           '
    trav2 = '&&VERIFG.RSUP'//'           '
    trav3 = '&&VERIFG.THET'//'           '
    call wkvect(trav0, 'V V K8', lobj2, iadrt0)
    call wkvect(trav1, 'V V R', lobj2, iadrt1)
    call wkvect(trav2, 'V V R', lobj2, iadrt2)
    call wkvect(trav3, 'V V R', (nbre+1)*lobj2, iadrt3)
!
    ndim=1
!
! ALLOCATION D'UN AUTRE OBJET DE TRAVAIL
!
    trav = '&&VERIFG.'//motfac(1:l)
    trav5 = '&&VERIFG.'//motfac(1:l2)//'2'
    call wkvect(trav, 'V V K8', ndim, jjj)
    call wkvect(trav5, 'V V K24', ndim, jjj2)
!
    nbpar = 1
    nompar(1) = 'ABSC'
!
!     CALCUL DES ABSCISSES CURVILIGNES LE LONG DU FOND DE FISSURE
!
    call gabscu(lobj2, coorn, nomno, chfond, xl,&
                trav4)
    call jeveuo(trav4, 'L', iadabs)
!
    do iocc = 1, nocc
!
        call getvr8(motfac(1:l), 'MODULE', iocc=iocc, scal=thet, nbret=nbm)
        if (nbm .ne. 1) then
            thet = 1.d0
        endif
        call getvr8(motfac(1:l), 'R_INF', iocc=iocc, scal=rinf, nbret=nbm)
        call getvr8(motfac(1:l), 'R_SUP', iocc=iocc, scal=rsup, nbret=nbm)
        if (nbm .ne. 0 .and. rsup .le. rinf) then
            call utmess('F', 'RUPTURE1_6')
        endif
        call getvid(motfac(1:l), 'MODULE_FO', iocc=iocc, scal=thetf, nbret=nbmof)
        call getvid(motfac(1:l), 'R_INF_FO', iocc=iocc, scal=rinff, nbret=nbmf)
        call getvid(motfac(1:l), 'R_SUP_FO', iocc=iocc, scal=rsupf, nbret=nbmf)
!
!       RECUPERATION DE RINF ET DE RSUP DANS LA SD FOND_FISS
        if (nbm .eq. 0 .and. nbmf .eq. 0) then
!
            if (config .eq. 'DECOLLEE') then
                call utmess('F', 'RUPTURE1_7')
            endif
            call jeveuo(taillr, 'L', iatmno)
            maxtai = 0.d0
            mintai = zr(iatmno)
            do j = 1, lobj2
                maxtai = max(maxtai,zr(iatmno-1+j))
                mintai = min(mintai,zr(iatmno-1+j))
            enddo
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
        nto=1
!
        do ito = 1, nto
            do j = 1, lobj2
                zk8(iadrt0 + j - 1) = zk8(iadrno + j - 1)
                noeud = zk8(iadrno + j - 1)
                if (nbmf .ne. 0) then
                    call jenonu(jexnom(nomno, zk8(iadrno+j-1)), num)
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
                    if (nbmof .ne. 0) then
                        call fointe('FM', thetf, nbpar, nompar, valpar,&
                                    valres, ier)
                    else
                        valres = 1.d0
                    endif
                    zr(iadrt3 + j - 1) = valres
                else
                    zr(iadrt1 + j - 1) = rinf
                    zr(iadrt2 + j - 1) = rsup
                    zr(iadrt3 + j - 1) = thet
                endif
            enddo
        enddo
    enddo
!
! VERIFICATION QUE GAMM0 EST COMPLET
!
    do i = 1, lobj2
        if (zk8(iadrno+ i -1) .ne. zk8(iadrt0+i-1)) then
            call utmess('F', 'RUPTURE1_9')
        endif
    end do
!
! DESTRUCTION D'OBJETS DE TRAVAIL
!
    call jedetr(trav)
    call jedetr(trav0)
    call jedetr(trav5)
!
    call jedema()
end subroutine
