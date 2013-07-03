subroutine irmaes(idfimd, nomaas, nomamd, nbimpr, caimpi,&
                  modnum, nuanom, nomtyp, nnotyp, sdcarm)
    implicit none
!
#include "asterc/indik8.h"
#include "asterfort/cesexi.h"
#include "asterfort/exisd.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/as_mmhcyw.h"
#include "asterfort/as_mmhraw.h"
#include "asterfort/u2mesg.h"
#include "asterfort/wkvect.h"
    integer :: ntymax
    parameter (ntymax = 69)
!
    character(len=8) :: nomaas, nomtyp(*), sdcarm
    character(len=64) :: nomamd
    integer :: nbimpr, caimpi(10, nbimpr), modnum(ntymax)
    integer :: nnotyp(*), nuanom(ntymax, *)
    integer :: idfimd, nvtyge
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
! person_in_charge: nicolas.sellenet at edf.fr
! ----------------------------------------------------------------------
!  IMPR_RESU - IMPRESSION DANS LE MAILLAGE DES ELEMENTS DE STRUCTURE
!  -    -                         --           -           -
! ----------------------------------------------------------------------
!
! IN  :
!   IDFIMD  K*   ENTIER LIE AU FICHIER MED OUVERT
!   NOMAAS  K8   NOM DU MAILLAGE ASTER
!   NOMAMD  K*   NOM DU MAILLAGE MED
!   NBIMPR  K*   NOMBRE D'IMPRESSIONS
!   CAIMPI  K*   ENTIERS POUR CHAQUE IMPRESSION
!   MODNUM  I(*) INDICATEUR SI LA SPECIFICATION DE NUMEROTATION DES
!                NOEUDS DES MAILLES EST DIFFERENTES ENTRE ASTER ET MED
!   NUANOM  I*   TABLEAU DE CORRESPONDANCE DES NOEUDS MED/ASTER.
!                NUANOM(ITYP,J): NUMERO DANS ASTER DU J IEME NOEUD DE LA
!                MAILLE DE TYPE ITYP DANS MED.
!   NOMTYP  K8(*)NOM DES TYPES POUR CHAQUE MAILLE
!   NNOTYP  I(*) NOMBRE DE NOEUDS PAR TYPES DE MAILLES
!   SDCARM  K*   SD_CARA_ELEM EN CHAM_ELEM_S
!
!
#include "jeveux.h"
!
!
    integer :: codret, ipoin, ityp, letype, ino, iret, nbcmp, iad
    integer :: jcnxma(ntymax), ima, nbsect, nbfibr, nbcouc
    integer :: nbmail, jtypma, jpoin, jconx, nmatyp(ntymax), icmpse
    integer :: jattma(ntymax), jccesv, jccesl, jccesd, jccesc
    integer :: edfuin, edelst, ednoda, jocesv, jocesl, jocesd
    integer :: jocesc, jorima(ntymax), icmpor, jpcesv, jpcesl, jpcesd
    integer :: jpcesc, icmpr1, icmpep, jrmin(ntymax), jrmax(ntymax)
    parameter   (edfuin=0)
    parameter   (edelst=5)
    parameter   (ednoda=0)
!
    character(len=1) :: k1bid
    character(len=8) :: saux08
    character(len=64) :: atepai, atangv, atrmax, atrmin
    parameter   (atepai = 'EPAISSEUR')
    parameter   (atangv = 'ANGLE DE VRILLE')
    parameter   (atrmin = 'RAYON MIN')
    parameter   (atrmax = 'RAYON MAX')
!
    logical :: exicoq, exituy, exipmf
!
    call jeveuo(nomaas//'.TYPMAIL', 'L', jtypma)
    call jelira(nomaas//'.NOMMAI', 'NOMUTI', nbmail, k1bid)
    call jeveuo(jexatr(nomaas//'.CONNEX', 'LONCUM'), 'L', jpoin)
    call jeveuo(nomaas//'.CONNEX', 'L', jconx)
!
    exicoq = .false.
    call exisd('CHAM_ELEM_S', sdcarm//'.CARCOQUE', iret)
    if (iret .ne. 0) then
        exicoq = .true.
        call jeveuo(sdcarm//'.CARCOQUE  .CESV', 'L', jccesv)
        call jeveuo(sdcarm//'.CARCOQUE  .CESL', 'L', jccesl)
        call jeveuo(sdcarm//'.CARCOQUE  .CESD', 'L', jccesd)
        call jeveuo(sdcarm//'.CARCOQUE  .CESC', 'L', jccesc)
        nbcmp = zi(jccesd+1)
        icmpse = indik8(zk8(jccesc),'EP',1,nbcmp)
    endif
!
    exituy = .false.
    call exisd('CHAM_ELEM_S', sdcarm//'.CARGEOPO', iret)
    if (iret .ne. 0) then
        exituy = .true.
        call jeveuo(sdcarm//'.CARGEOPO  .CESV', 'L', jpcesv)
        call jeveuo(sdcarm//'.CARGEOPO  .CESL', 'L', jpcesl)
        call jeveuo(sdcarm//'.CARGEOPO  .CESD', 'L', jpcesd)
        call jeveuo(sdcarm//'.CARGEOPO  .CESC', 'L', jpcesc)
        nbcmp = zi(jpcesd+1)
        icmpr1 = indik8(zk8(jpcesc),'R1',1,nbcmp)
        icmpep = indik8(zk8(jpcesc),'EP1',1,nbcmp)
        if (icmpr1 .eq. 0 .or. icmpep .eq. 0) exituy = .false.
    endif
!
    exipmf = .false.
    call exisd('CHAM_ELEM_S', sdcarm//'.CAFIBR', iret)
    if (iret .ne. 0) then
        exipmf = .true.
    endif
!
    if (exituy .or. exipmf) then
        call jeveuo(sdcarm//'.CARORIEN  .CESV', 'L', jocesv)
        call jeveuo(sdcarm//'.CARORIEN  .CESL', 'L', jocesl)
        call jeveuo(sdcarm//'.CARORIEN  .CESD', 'L', jocesd)
        call jeveuo(sdcarm//'.CARORIEN  .CESC', 'L', jocesc)
        nbcmp = zi(jocesd+1)
        icmpor = indik8(zk8(jocesc),'GAMMA',1,nbcmp)
    endif
!
!     -- DECOMPTE DU NOMBRE DE MAILLES PAR TYPE
    do 211 , ityp = 1 , ntymax
    nmatyp(ityp) = 0
    211 end do
!
    do 212 , ima = 1, nbmail
    nmatyp(zi(jtypma-1+ima)) = nmatyp(zi(jtypma-1+ima)) + 1
    212 end do
!
!     -- CREATION D'UN VECTEURS PAR TYPE DE MAILLE PRESENT CONTENANT
!          LA CONNECTIVITE DES MAILLE/TYPE
!          (CONNECTIVITE = NOEUDS + UNE VALEUR BIDON(0) SI BESOIN)
    do 23 , ityp = 1, ntymax
    if (nmatyp(ityp) .ne. 0) then
        call wkvect('&&IRMAES.CNX.'//nomtyp(ityp), 'V V I', nnotyp(ityp)*nmatyp(ityp),&
                    jcnxma(ityp))
        if (exicoq) then
            call wkvect('&&IRMAES.EPAI.'//nomtyp(ityp), 'V V R', nmatyp(ityp), jattma(ityp))
        endif
        if (exipmf .or. exituy) then
            call wkvect('&&IRMAES.ORIE.'//nomtyp(ityp), 'V V R', nmatyp(ityp), jorima(ityp))
        endif
        if (exituy) then
            call wkvect('&&IRMAES.RMIN.'//nomtyp(ityp), 'V V R', nmatyp(ityp), jrmin(ityp))
            call wkvect('&&IRMAES.RMAX.'//nomtyp(ityp), 'V V R', nmatyp(ityp), jrmax(ityp))
        endif
    endif
    23 end do
!
!     -- ON PARCOURT TOUTES LES MAILLES. POUR CHACUNE D'ELLES, ON
!          STOCKE SA CONNECTIVITE
!          LA CONNECTIVITE EST FOURNIE EN STOCKANT TOUS LES NOEUDS A
!          LA SUITE POUR UNE MAILLE DONNEE.
!          C'EST CE QU'ON APPELLE LE MODE ENTRELACE DANS MED
!          A LA FIN DE CETTE PHASE, NMATYP CONTIENT LE NOMBRE DE MAILLES
!          POUR CHAQUE TYPE
    do 241 , ityp = 1 , ntymax
    nmatyp(ityp) = 0
    241 end do
!
    do 242 , ima = 1, nbmail
!
    ityp = zi(jtypma-1+ima)
!
    ipoin = zi(jpoin-1+ima)
    nmatyp(ityp) = nmatyp(ityp) + 1
!
    if (exicoq) then
        call cesexi('C', jccesd, jccesl, ima, 1,&
                    1, icmpse, iad)
        if (iad .gt. 0) then
            zr(jattma(ityp)+nmatyp(ityp)-1) = zr(jccesv-1+iad)
        else
            zr(jattma(ityp)+nmatyp(ityp)-1) = 0.d0
        endif
    endif
    if (exipmf .or. exituy) then
        call cesexi('C', jocesd, jocesl, ima, 1,&
                    1, icmpor, iad)
        if (iad .gt. 0) then
            zr(jorima(ityp)+nmatyp(ityp)-1) = zr(jocesv-1+iad)
        else
            zr(jorima(ityp)+nmatyp(ityp)-1) = 0.d0
        endif
    endif
    if (exituy) then
        call cesexi('C', jpcesd, jpcesl, ima, 1,&
                    1, icmpr1, iad)
        if (iad .gt. 0) then
            zr(jrmin(ityp)+nmatyp(ityp)-1) = zr(jpcesv-1+iad)
        else
            zr(jrmin(ityp)+nmatyp(ityp)-1) = 0.d0
        endif
        call cesexi('C', jpcesd, jpcesl, ima, 1,&
                    1, icmpep, iad)
        if (iad .gt. 0) then
            zr(jrmax(ityp)+nmatyp(ityp)-1) = zr( jrmin(ityp)+ nmatyp(ityp)-1)+zr(jpcesv-1+iad )
        else
            zr(jrmax(ityp)+nmatyp(ityp)-1) = 0.d0
        endif
    endif
!       CONNECTIVITE DE LA MAILLE TYPE ITYP DANS VECT CNX:
!       I) POUR LES TYPES DE MAILLE DONT LA NUMEROTATION DES NOEUDS
!          ENTRE ASTER ET MED EST IDENTIQUE:
    if (modnum(ityp) .eq. 0) then
        do 2421 , ino = 1, nnotyp(ityp)
        zi(jcnxma(ityp)-1+(nmatyp(ityp)-1)*nnotyp(ityp)+ino) =&
                zi(jconx-1+ipoin-1+ino)
2421      continue
!       II) POUR LES TYPES DE MAILLE DONT LA NUMEROTATION DES NOEUDS
!          ENTRE ASTER ET MED EST DIFFERENTE (CF LRMTYP):
    else
        do 2422 , ino = 1, nnotyp(ityp)
        zi(jcnxma(ityp)-1+(nmatyp(ityp)-1)*nnotyp(ityp)+ino) =&
                zi(jconx-1+ipoin-1+nuanom(ityp,ino))
2422      continue
    endif
!
    242 end do
!
!     -- ECRITURE
    do 31, letype = 1, nbimpr
!
!       -- PASSAGE DU NUMERO DE TYPE MED AU NUMERO DE TYPE ASTER
    ityp = caimpi(8,letype)
    nvtyge = caimpi(9,letype)
    nbcouc = caimpi(4,letype)
    nbsect = caimpi(5,letype)
    nbfibr = caimpi(6,letype)
!
    if (nmatyp(ityp) .ne. 0 .and. (nbcouc.ne.0.or. nbsect.ne.0.or. nbfibr.ne.0)) then
!
!         -- LES CONNECTIVITES
!          LA CONNECTIVITE EST FOURNIE EN STOCKANT TOUS LES NOEUDS A
!          LA SUITE POUR UNE MAILLE DONNEE.
!          C'EST CE QUE MED APPELLE LE MODE ENTRELACE
        call as_mmhcyw(idfimd, nomamd, zi(jcnxma(ityp)), nnotyp(ityp)* nmatyp(ityp), edfuin,&
                    nmatyp(ityp), edelst, nvtyge, ednoda, codret)
        if (codret .ne. 0) then
            saux08='mmhcyw'
            call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                        codret, 0, 0.d0)
        endif
!
!         -- ATTRIBUTS VARIABLE, ICI L'EPAISSEUR
        if (nbcouc .ne. 0 .and. nbsect .eq. 0) then
            call as_mmhraw(idfimd, nomamd, nvtyge, atepai, nmatyp(ityp),&
                        zr(jattma(ityp)), codret)
            if (codret .ne. 0) then
                saux08='mmhraw'
                call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                            codret, 0, 0.d0)
            endif
        endif
!
!         -- ATTRIBUTS VARIABLE, ICI GAMMA
        if (nbfibr .ne. 0 .or. nbsect .ne. 0) then
            call as_mmhraw(idfimd, nomamd, nvtyge, atangv, nmatyp(ityp),&
                        zr(jorima(ityp)), codret)
            if (codret .ne. 0) then
                saux08='mmhraw'
                call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                            codret, 0, 0.d0)
            endif
        endif
!
!         -- ATTRIBUTS VARIABLE, ICI RMIN ET RMAX
        if (nbsect .ne. 0) then
            call as_mmhraw(idfimd, nomamd, nvtyge, atrmin, nmatyp(ityp),&
                        zr(jrmin(ityp)), codret)
            if (codret .ne. 0) then
                saux08='mmhraw'
                call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                            codret, 0, 0.d0)
            endif
            call as_mmhraw(idfimd, nomamd, nvtyge, atrmax, nmatyp(ityp),&
                        zr(jrmax(ityp)), codret)
            if (codret .ne. 0) then
                saux08='mmhraw'
                call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                            codret, 0, 0.d0)
            endif
        endif
!
    endif
!
    31 end do
!
    do 41 , ityp = 1, ntymax
    if (nmatyp(ityp) .ne. 0) then
        call jedetr('&&IRMAES.CNX.'//nomtyp(ityp))
        call jedetr('&&IRMAES.EPAI.'//nomtyp(ityp))
        if (exipmf .or. exituy) call jedetr('&&IRMAES.ORIE.'// nomtyp(ityp))
        if (exituy) then
            call jedetr('&&IRMAES.RMIN.'//nomtyp(ityp))
            call jedetr('&&IRMAES.RMAX.'//nomtyp(ityp))
        endif
    endif
    41 end do
!
end subroutine
