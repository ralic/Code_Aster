subroutine orilma(noma, ndim, listma, nbmail, norien,&
                  ntrait, reorie, nbmavo, mailvo)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/oriema.h"
#include "asterfort/utmasu.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    integer :: ndim, listma(*), nbmail, norien, ntrait, nbmavo, mailvo(*)
    character(len=8) :: noma
    logical :: reorie
!.======================================================================
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
!
!     ORILMA  --  LE BUT EST DE REORIENTER, SI C'EST NECESSAIRE,
!                 LES MAILLES DE PEAU D'UNE LISTE DE MAILLES
!                 LA NORMALE A LA MAILLE DE PEAU DOIT ETRE
!                 EXTERIEURE AU VOLUME.
!                 DANS LE CAS OU REORIE EST FAUX, L'ORIENTATION
!                 GEOMETRIQUE N'EST PAS UTILISEE, CECI PERMET DE
!                 TESTER UNE SURFACE POUR UNE CONDITION AUX LIMITES
!                 DE PRESSION
!
!   ARGUMENT        E/S  TYPE         ROLE
!    NOMA           IN    K8      NOM DU MAILLAGE
!    NDIM           IN    I       DIMENSION DU PROBLEME
!    LISTMA         IN    I       LISTE DES NUMEROS DE MAILLE
!                                 A REORIENTER
!    NBMAIL         IN    I       NB DE MAILLES DE LA LISTE
!    NORIEN        VAR            NOMBRE DE MAILLES REORIENTEES
!    REORIE         IN    L       INDIQUE SI L'ON DOIT APPELER ORIEMA
!    MAILVO         IN    I       SI ORIE_PEAU_3D ("GROUP_MA_VOLU")
!                                   = LISTE DES MAILLES VOLUMIQUES
!                                     UTILES A LA REORIENTATION
!                                 SI ORIE_PEAU_2D ("GROUP_MA_SURF")
!                                   = LISTE DES MAILLES SURFACIQUES
!                                     UTILES A LA REORIENTATION
!                                 SINON: MAILVO N'EST PAS UTILISE
!    NBMAVO         IN    I       NB DE MAILLES DE MAILVO
!.========================= DEBUT DES DECLARATIONS ====================
! -----  VARIABLES LOCALES
    integer :: ifm, niv, ima, numa, nutyma, nbnmai, numa3d, noriem, norieg
    integer :: jtyma, jcoor, p1, p2,     jm3d, jdesm, jdes3d
    logical :: dime1, dime2
    character(len=2) :: kdim
    character(len=8) :: tpmail, nomail, typ3d
    character(len=24) :: mailma, nomob1
    character(len=24) :: valk(2)
    integer, pointer :: ori1(:) => null()
    integer, pointer :: ori2(:) => null()
    character(len=8), pointer :: ori3(:) => null()
    character(len=8), pointer :: ori4(:) => null()
!
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
    call jemarq()
    if (nbmail .eq. 0) goto 9999
!
! --- INITIALISATIONS :
!     ---------------
    call infniv(ifm, niv)
    mailma = noma//'.NOMMAI'
!
! --- VECTEUR DU TYPE DES MAILLES DU MAILLAGE :
!     ---------------------------------------
    call jeveuo(noma//'.TYPMAIL        ', 'L', jtyma)
!
! --- COORDONNEES DES NOEUDS DU MAILLAGE :
!     ----------------------------------
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
!
! --- RECUPERATION DE LA CONNECTIVITE DES MAILLES :
!     -------------------------------------------
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', p2)
    call jeveuo(noma//'.CONNEX', 'E', p1)
!
!     ALLOCATIONS :
!     -----------
    AS_ALLOCATE(vi=ori1, size=nbmail)
    AS_ALLOCATE(vi=ori2, size=nbmail)
    AS_ALLOCATE(vk8=ori3, size=nbmail)
    AS_ALLOCATE(vk8=ori4, size=nbmail)
!
! --- VERIFICATION DU TYPE DES MAILLES
! --- (ON DOIT AVOIR DES MAILLES DE PEAU) :
!     -----------------------------------
    dime1 = .false.
    dime2 = .false.
    do 10 ima = 1, nbmail
        numa = listma(ima)
        call jenuno(jexnum(mailma, numa), nomail)
        ori3(ima) = nomail
        ori1(ima) = zi(p2+numa+1-1) - zi(p2+numa-1)
        ori2(ima) = zi(p2+numa-1)
!
! ---   TYPE DE LA MAILLE COURANTE :
!       --------------------------
        nutyma = zi(jtyma+numa-1)
        call jenuno(jexnum('&CATA.TM.NOMTM', nutyma), tpmail)
        ori4(ima) = tpmail
!
        if (tpmail(1:4) .eq. 'QUAD') then
            dime2 = .true.
        else if (tpmail(1:4).eq.'TRIA') then
            dime2 = .true.
        else if (tpmail(1:3).eq.'SEG') then
            dime1 = .true.
        else
            valk(1) = nomail
            valk(2) = tpmail
            call utmess('F', 'MODELISA5_94', nk=2, valk=valk)
        endif
        if (dime1 .and. dime2) then
            call utmess('F', 'MODELISA5_98')
        endif
!
10  end do
!
! --- RECHERCHE DES MAILLES SUPPORTS
!
    kdim =' '
    if (dime1) kdim ='2D'
    if (dime2) kdim ='3D'
    ASSERT(kdim.ne.' ')
    nomob1 = '&&ORILMA.MAILLE_3D'
    call utmasu(noma, kdim, nbmail, listma, nomob1,&
                zr(jcoor), nbmavo, mailvo, .false.)
    call jeveuo(nomob1, 'L', jm3d)
!
    norieg = 0
    ntrait = 0
!
    do 100 ima = 1, nbmail
!
        nomail = ori3(ima)
        tpmail = ori4(ima)
        nbnmai = ori1(ima)
        jdesm = ori2(ima)
        numa3d = zi(jm3d-1+ima)
        if (numa3d .eq. 0) then
            ntrait = ntrait + 1
            goto 100
        endif
        jdes3d = zi(p2+numa3d-1)
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(jtyma+numa3d-1)), typ3d)
!
        call oriema(nomail, tpmail, nbnmai, zi(p1+jdesm-1), typ3d,&
                    zi(p1+jdes3d-1), ndim, zr(jcoor), reorie, noriem,&
                    ifm, niv)
!
        norieg = norieg + noriem
!
100  end do
!
    norien = norien + norieg
!
    AS_DEALLOCATE(vi=ori1)
    AS_DEALLOCATE(vi=ori2)
    AS_DEALLOCATE(vk8=ori3)
    AS_DEALLOCATE(vk8=ori4)
    call jedetr(nomob1)
!
9999  continue
    call jedema()
end subroutine
