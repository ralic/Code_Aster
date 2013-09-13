subroutine xajuls(noma, nbma, cnslt, cnsln, jconx1,&
                  jconx2, clsm)
    implicit none
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/conare.h"
#include "asterfort/dismoi.h"
#include "asterfort/ismali.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
!
    integer :: nbma, jconx1, jconx2, clsm
    character(len=8) :: noma
    character(len=19) :: cnslt, cnsln
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
! person_in_charge: patrick.massin at edf.fr
!
!     ------------------------------------------------------------------
!     XFEM : REAJUSTEMENT DES LEVEL SETS (BOOK III 06/02/04)
!     -        ---
!     BUT : ON MODIFIE LES VALEURS DE LS AUX NOEUDS SI TROP
!           PROCHES DE 0 POUR EVITER LES ERREURS D'INTEGRATION
!
!    ENTREE :
!              IFM    :   FICHIER D'IMPRESSION
!              NOMA   :   OBJET MAILLAGE
!              NBMA   :   NOMBRE DE MAILLES DU MAILLAGE
!              CNSLN  :   LEVEL-SET NORMALE  (PLAN DE LA FISSURE)
!              CNSLT  :   LEVEL-SET TANGENTE (TRACE DE LA FISSURE)
!       JCONX1,JCONX2 :   INDICES DE LA CONNECTIVITE
!
!    SORTIE :
!              CNSLN  :   LEVEL-SET NORMALE
!              CNSLT  :   LEVEL-SET TANGENTE
!              CLSM   :   NOMBRE DE LEVEL SETS MODIFIEES
!
!     ------------------------------------------------------------------
!
    integer :: jma, ima, itypma, ar(12, 3), nbar, ia
    integer :: na, nb, nm, nunoa, nunob, nunom
    integer :: jlnsv, jltsv, nmaabs, ndime, ndim, iret
    real(kind=8) :: d1, lsna, lsnb, crilsn, lsta, lstb, crilst, d2
    real(kind=8) :: lsnm, lstm, lsnmax, lstmax
    character(len=19) :: mai
    character(len=8) :: typma
    character(len=32) :: kbid
!
    parameter     (crilsn=1.d-2, crilst=1.d-3)
!
!-----------------------------------------------------------------------
!     DEBUT
!-----------------------------------------------------------------------
    call jemarq()
!
    call jeveuo(cnsln//'.CNSV', 'E', jlnsv)
    call jeveuo(cnslt//'.CNSV', 'E', jltsv)
!
    d2=999.d0
!
!     COMPTEUR DES LSN ET LST MODIFIÉES
    clsm=0
    mai=noma//'.TYPMAIL'
    call jeveuo(mai, 'L', jma)
!
!     RECUPERATION DE LA DIMENSION DE L'ESPACE
    call dismoi('F', 'DIM_GEOM', noma, 'MAILLAGE', ndime,&
                kbid, iret)
!
!     BOUCLE SUR TOUTES LES MAILLES DU MAILLAGE
!
    do 200 ima = 1, nbma
        nmaabs=ima
        itypma=zi(jma-1+ima)
        call jenuno(jexnum('&CATA.TM.NOMTM', itypma), typma)
!
!       RECUPERATION DE LA DIMENSION TOPOLOGIQUE DE L'ELEMENT
        call dismoi('F', 'DIM_TOPO', typma, 'TYPE_MAILLE', ndim,&
                    kbid, iret)
!
!       LES ELEMENTS DE BORD NE SONT PAS TRAITES
        if (ndim .lt. ndime) goto 200
!
!       BOUCLE SUR LES ARETES DE LA MAILLE VOLUMIQUE
        call conare(typma, ar, nbar)
        lsnmax=0.d0
        lstmax=0.d0
!
!       BOUCLE POUR RECUPERER LE MAX DE ABS(LSN) SUR UNE MAILLE
!       (UNIQUEMENT LES NOEUDS SOMMETS) ET FIT TO VERTEX LES NOEUDS
!       SOMMETS
!
        do 210 ia = 1, nbar
            na=ar(ia,1)
            nb=ar(ia,2)
            nunoa=zi(jconx1-1+zi(jconx2+nmaabs-1)+na-1)
            nunob=zi(jconx1-1+zi(jconx2+nmaabs-1)+nb-1)
            lsna=zr(jlnsv-1+(nunoa-1)+1)
            lsnb=zr(jlnsv-1+(nunob-1)+1)
            lsta=zr(jltsv-1+(nunoa-1)+1)
            lstb=zr(jltsv-1+(nunob-1)+1)
            if (abs(lsna) .gt. lsnmax) lsnmax=abs(lsna)
            if (abs(lsnb) .gt. lsnmax) lsnmax=abs(lsnb)
            if (abs(lsta) .gt. lstmax) lstmax=abs(lsta)
            if (abs(lstb) .gt. lstmax) lstmax=abs(lstb)
!
!         REAJUSTEMENT DE LA LEVEL SET NORMALE AUX NOEUDS SOMMETS, QUAND
!         LA VALEUR D'UN LSN DIVISE PAR LA DIFFERENCE DES VALEURS AUX
!         DEUX EXTREMITES SONT INFERIEURES D'UN CERTAINE NOMBRE, ON MET
!         LES LSN A ZERO.
            if (abs(lsna-lsnb) .gt. r8prem()) then
                d1=lsna/(lsna-lsnb)
                if (abs(d1) .le. crilsn) then
!              REAJUSTEMENT DE LSNA
                    zr(jlnsv-1+(nunoa-1)+1)=0.d0
                    clsm=clsm+1
                endif
                if (abs(d1-1.d0) .le. (crilsn)) then
!              REAJUSTEMENT DE LSNB
                    zr(jlnsv-1+(nunob-1)+1)=0.d0
                    clsm=clsm+1
                endif
            endif
!
!         REAJUSTEMENT DE LA LEVEL SET TANGENTE AUX NOEUDS SOMMETS,
!         QUAND LA VALEUR D'UN LST DIVISE PAR LA DIFFERENCE DES VALEURS
!         AUX DEUX EXTREMITES SONT INFERIEURES D'UN CERTAINE NOMBRE, ON
!         MET LES LST A ZERO.
!
            if (abs(lsta-lstb) .gt. r8prem()) then
                d1=lsta/(lsta-lstb)
                if (abs(d1) .le. crilst) then
!              REAJUSTEMENT DE LSTA
                    zr(jltsv-1+(nunoa-1)+1)=0.d0
                    clsm=clsm+1
                endif
                if (abs(d1-1.d0) .le. (crilst)) then
!              REAJUSTEMENT DE LSTB
                    zr(jltsv-1+(nunob-1)+1)=0.d0
                    clsm=clsm+1
                endif
            endif
!
210      continue
!
        do 230 ia = 1, nbar
            na=ar(ia,1)
            nb=ar(ia,2)
            nunoa=zi(jconx1-1+zi(jconx2+nmaabs-1)+na-1)
            nunob=zi(jconx1-1+zi(jconx2+nmaabs-1)+nb-1)
!
            if (.not. ismali(typma)) then
                nm=ar(ia,3)
                nunom=zi(jconx1-1+zi(jconx2+nmaabs-1)+nm-1)
                lsnm=zr(jlnsv-1+(nunom-1)+1)
                lstm=zr(jltsv-1+(nunom-1)+1)
!
                if (zr(jlnsv-1+(nunoa-1)+1) .eq. 0.d0 .and. zr(jlnsv-1+( nunob-1)+1) .eq.&
                    0.d0) then
                    d2=lsnm/lsnmax
                    if (abs(d2) .le. crilsn) then
!             REAJUSTEMENT DE LSNM AUX NOEUDS MILIEUX,QUAND LA VALEUR
!             D'UN LSNM SUR LA VALEUR LSNM LE PLUS GRAND DANS LE MEME
!             ELEMENT EST INFERIEURE A UN CERTAIN NOMBRE, ON MET LES
!             LSN A ZERO.
                        zr(jlnsv-1+(nunom-1)+1)=0.d0
                        clsm=clsm+1
                    else
                        call utmess('A', 'XFEM_63')
                    endif
                endif
                if (zr(jltsv-1+(nunoa-1)+1) .eq. 0.d0 .and. zr(jltsv-1+( nunob-1)+1) .eq.&
                    0.d0) then
                    d2=lstm/lstmax
                    if (abs(d2) .le. crilst) then
!             REAJUSTEMENT DE LSTM AUX NOEUDS MILIEUX,QUAND LA VALEUR
!             D'UN LSTM SUR LA VALEUR LSTM LE PLUS GRAND DANS LE MEME
!             ELEMENT EST INFERIEURE A UN CERTAIN NOMBRE, ON MET LES
!             LST A ZERO.
                        zr(jltsv-1+(nunom-1)+1)=0.d0
                        clsm=clsm+1
                    else
                        call utmess('A', 'XFEM_63')
                    endif
                endif
            endif
!
230      continue
!
200  end do
!
!-----------------------------------------------------------------------
!     FIN
!-----------------------------------------------------------------------
    call jedema()
end subroutine
