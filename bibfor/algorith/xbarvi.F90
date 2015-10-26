subroutine xbarvi(noma, nomo, fiss, faclon, ainter, tabai)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/cesexi.h"
#include "asterfort/conare.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
#include "asterfort/xelfis_lists.h"
#include "asterfort/xxmmvd.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    character(len=8) :: noma, nomo, fiss
    character(len=19) :: faclon, ainter, tabai
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
! ----------------------------------------------------------------------
!
! ROUTINE XFEM
!
! C'EST ICI QUE L'ON REMPLIT LA 5EME COMPOSANTES DE TOPOFAC.AI
! DANS L'ELEMENT DE CONTACT XFEM
!
! CETTE COMPOSANTE VAUT :
!       1 SI L'ARETE INTERSECTÉE EST VITALE
!       0 SINON
!
! ON CRÉE AUSSI LA SD FISS(1:8)//'.CONTACT.CNCTE QUI LISTE ET RENSEIGNE
! LES ARETES APARTENANTES A UN GROUPE D'ARETES VITALES CONNECTÉES,
! IL Y A 4 COMPOSANTES PAR ARETES :
!      1 : NUMERO D'ARETE DANS LE GROUPE
!      2 : NUMERO DE GROUPE
!      3 : NUMERO DE MAILLE
!      4 : NUMERO LOCAL DE L'ARETE DANS LA MAILLE
!
! TRAVAIL EFFECTUE EN COLLABORATION AVEC L'I.F.P.
!
! ----------------------------------------------------------------------
!
! IN  CHAR      : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  NOMA      : NOM DU MAILLAGE
! IN  NOMO      : NOM DU MODELE
! IN  FISS      : NOM DE LA FISSURE EN COURS
! IN FACLON     : TOPOFAC.LO SIMPLIFIÉ
! IN/OUT AINTER : TOPOFAC.AI SIMPLIFIÉ
! IN TABAI      : 
!
!
!
    integer :: jcsd1, jcsl1, jcsd2, jcsl2, jconx2
    integer :: ima, pin, iad, iret, ninter, ifiss, dimai, nbedge
    integer :: ar(12, 3), i, ia, nbar, nloc(2), nglo(2), no
    character(len=24) :: grp(3), elfis_heav, elfis_ctip, elfis_hect
    character(len=8) :: typma
    character(len=6) :: nompro
    parameter (nompro = 'XBARVI')
    integer :: zxain, ier
    aster_logical :: lmulti
    integer :: igrp, jgrp, ienr, nmaenr, iar
    integer, pointer :: vtabai(:) => null()    
    integer, pointer :: connex(:) => null()
    integer, pointer :: typmail(:) => null()
    integer, pointer :: nbsp(:) => null()
    integer, pointer :: csv1(:) => null()
    real(kind=8), pointer :: csv2(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    zxain = xxmmvd('ZXAIN')
!
! --- LE MULTI-HEAVISIDE EST-IL ACTIF ?
!
    call jeexin(nomo//'.FISSNO    .CELD', ier)
    if (ier .eq. 0) then
        lmulti = .false.
        ifiss = 1
    else
        lmulti = .true.
        call jeveuo('&&XCONTA.NBSP', 'L', vi=nbsp)
    endif
!
! --- Le tableau tabai a été créé par xlagsp ?
!
    call jeexin(tabai, ier)
    if(ier.ne.0) then
        call jeveuo(tabai, 'L', vi=vtabai)
        call jelira(tabai, 'LONMAX', dimai)
        nbedge=dimai/3
    else 
        nbedge=0
    endif
!
! --- ON RECUPERE DES INFOS GLOBALES SUR LE MAILLAGE
!
    call jeveuo(noma(1:8)//'.TYPMAIL', 'L', vi=typmail)
    call jeveuo(noma(1:8)//'.CONNEX', 'L', vi=connex)
    call jeveuo(jexatr(noma(1:8)//'.CONNEX', 'LONCUM'), 'L', jconx2)
!
! --- ACCES AUX CHAM_ELEM_S
!
    call jeveuo(faclon//'.CESD', 'L', jcsd1)
    call jeveuo(faclon//'.CESV', 'L', vi=csv1)
    call jeveuo(faclon//'.CESL', 'L', jcsl1)
    call jeveuo(ainter//'.CESD', 'L', jcsd2)
    call jeveuo(ainter//'.CESL', 'E', jcsl2)
    call jeveuo(ainter//'.CESV', 'E', vr=csv2)
!
! --- RECUPERATION DES GROUPES
!
    elfis_heav='&&'//nompro//'.ELEMFISS.HEAV'
    elfis_ctip='&&'//nompro//'.ELEMFISS.CTIP'
    elfis_hect='&&'//nompro//'.ELEMFISS.HECT'
    call xelfis_lists(fiss, nomo, elfis_heav,&
                          elfis_ctip, elfis_hect)
    grp(1)=elfis_heav
    grp(2)=elfis_ctip
    grp(3)=elfis_hect
!
! --- Boucle sur les groupes de mailles
!
    do igrp = 1, 3
        call jeexin(grp(igrp), iret)
        if (iret .eq. 0) cycle
        call jeveuo(grp(igrp), 'L', jgrp)
        call jelira(grp(igrp), 'LONMAX', nmaenr)
!
! --- Boucle sur les mailles du groupe
!
        do ienr = 1, nmaenr
            ima = zi(jgrp-1+ienr)
            if (lmulti) ifiss = nbsp(ima)
            if (ifiss .eq. 0) cycle
            call cesexi('C', jcsd1, jcsl1, ima, 1, ifiss, 1, iad)
            if (iad .eq. 0) cycle
            ASSERT(iad.gt.0)
            ninter = csv1(iad)
            if (ninter .eq. 0) cycle
            ASSERT(ninter.gt.0)
            call jenuno(jexnum('&CATA.TM.NOMTM', typmail(ima)), typma)
            call conare(typma, ar, nbar)
!
! --- Boucle sur les arêtes intersectées
!
            do pin = 1, ninter
                call cesexi('S', jcsd2, jcsl2, ima, 1, ifiss, zxain*(pin-1)+ 1, iad)
                ASSERT(iad.gt.0)
                ia=nint(csv2(iad))
                call cesexi('S', jcsd2, jcsl2, ima, 1, ifiss, zxain*(pin-1)+ 2, iad)
                ASSERT(iad.gt.0)
                no=nint(csv2(iad))
                ASSERT(no.ge.0)
                call cesexi('S', jcsd2, jcsl2, ima, 1, ifiss, zxain*(pin-1)+ 5, iad)
                if (iad .le. 0) iad = -iad
!
! --- S IL S'AGIT D'UN NOEUD, ALORS IL EST VITAL
!
                if (no .gt. 0) then
                    csv2(iad) = 1
                    zl(jcsl2-1+iad) = .true.
                endif
!
! --- S IL S'AGIT D'UNE ARRETE, RECUP DES NUM GLOBAUX DE SES NOEUDS
!
                if (ia .gt. 0) then
                    do i = 1, 2
                        nloc(i) = ar(ia,i)
                        nglo(i) = connex(zi(jconx2+ima-1)+nloc(i)-1)
                    enddo
!
! --- Initialisation: on écrit 0 par défaut
!
                    csv2(iad) = 0
                    zl(jcsl2-1+iad) = .true.
!
! --- Recherche de l'arête pour récupérer son statut stocké dans tabai
!
                    do iar = 1, nbedge
                        if ((vtabai(3*(iar-1) + 1).eq.nglo(1) .and. &
                             vtabai(3*(iar-1) + 2).eq.nglo(2)) .or.&
                            (vtabai(3*(iar-1) + 1).eq.nglo(2) .and. &
                             vtabai(3*(iar-1) + 2).eq.nglo(1))) then
!                           'iar' est l'indice de l'arête recherchée. On copie les informations
                            csv2(iad) = vtabai(3*(iar-1) + 3)  
                        endif
                    enddo
!
                endif 
            enddo
        enddo
    enddo
!
!   menage
!
    do igrp = 1, 3
        call jeexin(grp(igrp), iret)
        if (iret .ne. 0) call jedetr(grp(igrp)) 
    enddo
!
    call jedetr(tabai)
    call jedetr(fiss(1:8)//'.PILO')
!
    call jedema()
end subroutine
