subroutine xprini(noma, cnxinv, grille, noesom, &
                  vcn, grlr, lcmin, ndim)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8gaem.h"
#include "asterfort/calcul.h"
#include "asterfort/celces.h"
#include "asterfort/conare.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupo.h"
#include "asterfort/jelira.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/padist.h"
#include "asterfort/wkvect.h"
#include "asterfort/xprcnu.h"
!
    character(len=8)  :: noma
    character(len=19) :: noesom, cnxinv
    character(len=24) :: vcn, grlr
    real(kind=8)      :: lcmin
    integer           :: ndim    
    aster_logical     :: grille

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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: patrick.massin at edf.fr
!     ------------------------------------------------------------------
!
!       XPRINI   : X-FEM PROPAGATION 
!
!    ENTREE
!        NOMA    : NOM DU CONCEPT MAILLAGE
!        CNXINV  : CONNECTIVITE INVERSEE DU MAILLAGE NOMA
!        GRILLE  : .TRUE. SI NOMA EST UNE GRILLE AUXILIAIRE
!                  .FALSE. SI NOMA N'EST PAS UNE GRILLE AUXILIAIRE

!
!    SORTIE
!        NOESOM  : VECTEUR LOGIQUE INDIQUANT SI LE NOEUD EST SOMMET
!
!    EN PLUS, SI GRILLE=.FALSE. ET SI LA METHODE UPWIND_FMM EST UTILISEE,
!    ON A ON SORTIE LES OBJETS SUIVANTES:
!        VCN     : VOIR XPRCNU.F POUR LA DESCRIPTION DE CETTE OBJET.
!                  POUR LA METHODE SIMPLEXE, CET OBJET N'EST PAS UTILISE
!        GRLR    : VOIR XPRCNU.F POUR LA DESCRIPTION DE CETTE OBJET
!                  POUR LA METHODE SIMPLEXE, CET OBJET N'EST PAS UTILISE
!
!    SI GRILLE=.FALSE.
!        LCMIN   : LONGUEUR DE LA PLUS PETITE ARETE DE LA GRILLE
!
!     ------------------------------------------------------------------
    
    integer                           :: jconx1, jconx2, itypma
    integer                           :: ino, ima, ifm, niv, nbno, nbma, jnosom, ibid
    integer                           :: ar(12, 3), nbar, iar, na, nb, nunoa, nunob
    character(len=8)                  :: method, nomno, typma
    real(kind=8)                      :: dist   
    real(kind=8),dimension(ndim)      :: xa, xb, v
    integer, pointer                  :: mai(:) => null()
    real(kind=8), pointer             :: vale(:) => null()        
!-----------------------------------------------------------------------
!     DEBUT
!-----------------------------------------------------------------------
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)
    

!   RETRIEVE THE DEFINITION OF THE ELEMENTS IN TERMS OF NODES
    call jeveuo(noma//'.CONNEX', 'L', jconx1)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
    
!   RETRIEVE THE TYPE OF EACH ELEMENT IN THE MESH
    call jeveuo(noma//'.TYPMAIL', 'L', vi=mai)    

!   RETRIEVE THE COORDINATES OF THE NODES
    call jeveuo(noma//'.COORDO    .VALE', 'L', vr=vale)      

!   RECUPERATION INFOS DU MAILLAGE
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbno)

    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbma)

!   RECUPERATION DE LA METHODE DE REINITIALISATION A EMPLOYER
    call getvtx(' ', 'METHODE', scal=method, nbret=ibid)
!

    if ( method .eq.'UPWIND' .and. (.not.grille)) then
        call xprcnu(noma, cnxinv, 'V', vcn, grlr, lcmin)
    endif
!
    if ( method.ne.'UPWIND' .and. (.not.grille)) then

        lcmin = r8gaem()

        ! boucle sur les mailles
        do ima = 1, nbma
            itypma=mai(ima)
            call jenuno(jexnum('&CATA.TM.NOMTM', itypma), typma)   
            call conare(typma, ar, nbar)
            do iar = 1, nbar
                na=ar(iar,1)
                nb=ar(iar,2)
                nunoa=zi(jconx1-1+(zi(jconx2+ima-1)+na-1))
                nunob=zi(jconx1-1+(zi(jconx2+ima-1)+nb-1))
                xa(1:ndim) = vale(3*(nunoa-1)+1:3*(nunoa-1)+ndim)
                xb(1:ndim) = vale(3*(nunob-1)+1:3*(nunob-1)+ndim)
                v=xb-xa
                dist = sqrt(dot_product(v, v))
                lcmin = min(lcmin,dist)
            enddo
        enddo
        
        print*, 'new : lcmin', lcmin
        
    else
        write(ifm,*)'   LONGUEUR DE LA PLUS PETITE ARETE DU MAILLAGE:'&
     &               //' ',lcmin
    endif
    
!------------------------------------------------------------------
!     ON REPERE LES NOEUDS SOMMETS (DONT LE GRADIENT DE LS EST NUL)
!------------------------------------------------------------------

    call wkvect(noesom, 'V V L', nbno, jnosom)
    do ino = 1, nbno
        zl(jnosom-1+ino) = .true.
        call jenuno(jexnum(noma //'.NOMNOE', ino), nomno)
        if (nomno(1:2) .eq. 'NS') zl(jnosom-1+ino) = .false.

    end do
    
!-----------------------------------------------------------------------
!     FIN
!-----------------------------------------------------------------------

    call jedema()
end subroutine
