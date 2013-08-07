subroutine uimpba(clas, iunmes)
    implicit none
#include "jeveux.h"
!
#include "asterc/gettco.h"
#include "asterfort/assert.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelgdq.h"
#include "asterfort/jelira.h"
#include "asterfort/jelstc.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
    character(len=*) :: clas
    integer :: iunmes
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
! person_in_charge: jacques.pellet at edf.fr
! ----------------------------------------------------------------------
! BUT:
!   IMPRIMER LA TAILLE DES CONCEPTS STOCKES SUR UNE BASE
!
!  IN    CLAS  : NOM DE LA BASE : 'G', 'V', ..(' ' -> TOUTES LES BASES)
! ----------------------------------------------------------------------
    character(len=8) :: k8
    character(len=24) :: kbid, obj
    character(len=16) :: typcon
    real(kind=8) :: rlong, mega, taitot
    integer :: i, nbobj, nbval, iexi, nbcon, jtailo, jtailc, nbsv
    integer :: jlobj, jnbobj, jnbsvo, jnbsvc, nstot
!
!
    mega=1024*1024
!
!
!     -- 1 : NBOBJ + .LISTE_OBJ :LISTE DES OBJETS :
!     ----------------------------------------------
    nbobj=0
    call jelstc(clas, ' ', 0, nbobj, kbid,&
                nbval)
    ASSERT(nbval.le.0)
    if (nbval .eq. 0) goto 9999
    nbobj=-nbval
    call wkvect('&&UIMPBA.LISTE_OBJ', 'V V K24', nbobj+1, jlobj)
    call jelstc(clas, ' ', 0, nbobj, zk24(jlobj),&
                nbval)
!     NBVAL = NBOBJ (+1 EVENTUELLEMENT A CAUSE DE '&&UIMPBA.LISTE_OBJ')
    ASSERT(nbval.eq.nbobj+1 .or. nbval.eq.nbobj)
    nbobj=nbval
!
!     -- 2 : .TAILLE = TAILLE DES OBJETS :
!     --------------------------------------
    call wkvect('&&UIMPBA.TAILLE', 'V V R', nbobj, jtailo)
    call wkvect('&&UIMPBA.NBSVO', 'V V I', nbobj, jnbsvo)
    do 1, i=1,nbobj
    obj=zk24(jlobj-1+i)
    call jelgdq(obj, rlong, nbsv)
    ASSERT(rlong.gt.0.d0)
    zr(jtailo-1+i)=rlong
    zi(jnbsvo-1+i)=nbsv
    1 end do
!
!
!     -- 3 : .LCONK8 = LISTE DES CONCEPTS (K8) DE .LISTE_OBJ
!     -----------------------------------------------------------
    call jecreo('&&UIMPBA.LCONK8', 'V N K8')
    call jeecra('&&UIMPBA.LCONK8', 'NOMMAX', nbobj)
    do 2, i=1,nbobj
    obj=zk24(jlobj-1+i)
    k8=obj(1:8)
    call jenonu(jexnom('&&UIMPBA.LCONK8', k8), iexi)
    if (iexi .eq. 0) then
        call jecroc(jexnom('&&UIMPBA.LCONK8', k8))
    endif
    2 end do
!
!
!     -- 4 : .TAILCON = TAILLE DES CONCEPTS
!     -----------------------------------------------------------
    call jelira('&&UIMPBA.LCONK8', 'NOMUTI', nbcon)
    call wkvect('&&UIMPBA.TAILCON', 'V V R', nbcon, jtailc)
    call wkvect('&&UIMPBA.NBSVC', 'V V I', nbcon, jnbsvc)
    call wkvect('&&UIMPBA.NBOBJ', 'V V I', nbcon, jnbobj)
    taitot=0.d0
    nstot=0
    do 3, i=1,nbobj
    obj=zk24(jlobj-1+i)
    k8=obj(1:8)
    call jenonu(jexnom('&&UIMPBA.LCONK8', k8), iexi)
    ASSERT(iexi.gt.0)
    ASSERT(iexi.le.nbcon)
    zr(jtailc-1+iexi)=zr(jtailc-1+iexi)+zr(jtailo-1+i)
    taitot=taitot+zr(jtailo-1+i)
    zi(jnbsvc-1+iexi)=zi(jnbsvc-1+iexi)+zi(jnbsvo-1+i)
    zi(jnbobj-1+iexi)=zi(jnbobj-1+iexi)+1
    nstot=nstot+zi(jnbsvo-1+i)
    3 end do
!
!
!     -- 5 : IMPRESSION DU RESULTAT :
!     -----------------------------------------------------------
    write(iunmes,*) '-----------------------------------------------',&
     &                '----------------------------'
    write(iunmes,*) 'Concepts de la base: ',clas
    write(iunmes,*) '   Nom       Type                 Taille (Mo)',&
     &                '         Nombre      Nombre de'
    write(iunmes,*) '                                            ',&
     &                '        d''objets       segments'
!
    write(iunmes,1000) 'TOTAL   ',' ',taitot/mega,nbobj,nstot
    write(iunmes,*) ' '
!
!     -- ON IMPRIME D'ABORD LES CONCEPTS UTILISATEUR :
    do 4, i=1,nbcon
    call jenuno(jexnum('&&UIMPBA.LCONK8', i), k8)
    call gettco(k8, typcon)
    if (typcon .eq. ' ') goto 4
    write(iunmes,1000) k8,typcon,zr(jtailc-1+i)/mega, zi(jnbobj-1+&
        i),zi(jnbsvc-1+i)
    4 end do
!     -- ON IMPRIME ENSUITE LES CONCEPTS CACHES  :
    do 5, i=1,nbcon
    call jenuno(jexnum('&&UIMPBA.LCONK8', i), k8)
    call gettco(k8, typcon)
    if (typcon .ne. ' ') goto 5
    write(iunmes,1000) k8,typcon,zr(jtailc-1+i)/mega, zi(jnbobj-1+&
        i),zi(jnbsvc-1+i)
    5 end do
    write(iunmes,*) '-----------------------------------------------',&
     &                '----------------------------'
!
!
9999  continue
    call jedetr('&&UIMPBA.LISTE_OBJ')
    call jedetr('&&UIMPBA.TAILLE')
    call jedetr('&&UIMPBA.LCONK8')
    call jedetr('&&UIMPBA.TAILCON')
    call jedetr('&&UIMPBA.NBSVO')
    call jedetr('&&UIMPBA.NBSVC')
    call jedetr('&&UIMPBA.NBOBJ')
    1000 format (4x,a8,3x,a16,3x,f12.2,3x,i12,3x,i12)
end subroutine
