subroutine cmqlno(main, maout, nbnm, nunomi)
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
    implicit none
#include "jeveux.h"
!
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupo.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    integer :: nbnm, nunomi(nbnm)
    character(len=8) :: main, maout
!-----------------------------------------------------------------------
!    - COMMANDE :  CREA_MAILLAGE / QUAD_LINE
!    - BUT DE LA COMMANDE:
!      TRANSFORMATION DES MAILLES QUADRATIQUES -> LINEAIRES
!    - BUT DE LA ROUTINE: CREATION DES OBJETS
!         '.NOMNOE' , '.COORDO', '.NOMNOE', '.GROUPENO'
!    - ROUTINE APPELEE PAR : CMQLQL
! ----------------------------------------------------------------------
! IN        MAIN   K8   NOM DU MAILLAGE INITIAL
! IN        MAOUT  K8   NOM DU MAILLAGE FINAL
! IN        NBNM    I   NOMBRE DE NOEUDS MILIEUX
! IN        NUNOMI K8   NUMEROS DES NOEUDS MILIEUX
! ----------------------------------------------------------------------
!
!
    integer :: jdim, nbtno, jnon, i, inoeu, nbno, nbtgno, nbnogr, jname
    integer ::  j, jgnoi, jgnou, kk,   iret, ij
    character(len=8) :: nnoi
    character(len=19) :: coordo, coordi
    character(len=24) :: nom, nomnoi, nomnoe, grpno, dime, gpptnn
    integer, pointer :: noeud_group(:) => null()
    real(kind=8), pointer :: val1(:) => null()
    real(kind=8), pointer :: val2(:) => null()
    character(len=24), pointer :: refe(:) => null()
!
    call jemarq()
!
    nomnoe = maout//'.NOMNOE'
    nomnoi = main //'.NOMNOE'
    grpno = maout//'.GROUPENO'
    coordo = maout//'.COORDO'
    coordi = main //'.COORDO'
    dime = maout//'.DIME'
!
!     CREATION D'UN TABLEAU DIMENSIONNE AU NOMBRE DE NOEUDS DU
!     MAILLAGE INITIAL PERMETTANT DE SAVOIR SI LE NOEUD SERA
!     PRESENT OU NON DANS LA NOUVELLE SD MAILLAGE.
!     --------------------------------------------
    call jeveuo(main//'.DIME', 'L', jdim)
    nbtno=zi(jdim)
    call wkvect('&&CMQLNO_NOEUD', 'V V I', nbtno, jnon)
    do 10 i = 1, nbtno
        zi(jnon+i-1)=0
10  end do
    do 20 i = 1, nbnm
        zi(jnon+nunomi(i)-1)=1
20  end do
!
!     RECUPERATION DES NOMS DES NOEUDS
!     --------------------------------
    j=0
    call wkvect('&&CMQLNO.NOM_NOEUDS', 'V V K24', zi(jdim), jname)
    do 30 i = 1, nbtno
        call jenuno(jexnum(nomnoi, i), nom)
        if (zi(jnon+i-1) .eq. 0) then
            j=j+1
            zk24(jname+j-1)=nom
        endif
30  end do
    nbno=j
!
!     CREATION DE L'OBJET '.NOMNOE'
!     ----------------------------
    call jecreo(nomnoe, 'G N K8')
    call jeveuo(dime, 'L', jdim)
    call jeecra(nomnoe, 'NOMMAX', nbno)
    do 31 i = 1, nbno
        call jecroc(jexnom(nomnoe, zk24(jname+i-1)))
31  end do
!
!
!     CREATION DE L'OBJET '.COORDO'
!     ----------------------------
    call jedupo(coordi//'.DESC', 'G', coordo//'.DESC', .false._1)
    call jedupo(coordi//'.REFE', 'G', coordo//'.REFE', .false._1)
    call jeveuo(coordo//'.REFE', 'E', vk24=refe)
    refe(1) = maout
    call jecreo(coordo//'.VALE', 'G V R')
    call jeecra(coordo//'.VALE', 'LONMAX', nbno*3)
    call jeveuo(main//'.COORDO    .VALE', 'L', vr=val1)
    call jeveuo(coordo//'.VALE', 'E', vr=val2)
    do 40 i = 1, nbno
        call jenuno(jexnum(nomnoe, i), nom)
        call jenonu(jexnom(nomnoi, nom), inoeu)
        val2(1+3*(i-1) )=val1(1+3*(inoeu-1) )
        val2(1+3*(i-1)+1)=val1(1+3*(inoeu-1)+1)
        val2(1+3*(i-1)+2)=val1(1+3*(inoeu-1)+2)
40  end do
!
!
!     CREATION DE L'OBJET '.GROUPENO'
!     -------------------------------
    call jeexin(main//'.GROUPENO', iret)
!
    if (iret .ne. 0) then
        call jelira(main//'.GROUPENO', 'NMAXOC', nbtgno)
        gpptnn = maout//'.PTRNOMNOE'
        call jecreo(gpptnn, 'G N K24')
        call jeecra(gpptnn, 'NOMMAX', nbtgno)
        call jecrec(grpno, 'G V I', 'NO '//gpptnn, 'DISPERSE', 'VARIABLE',&
                    nbtgno)
        do 50 i = 1, nbtgno
!
!           ON RECUPERE LES NOEUDS DU GROUPE QUI DOIVENT ETRE PRESENTS
!           DANS LA NOUVELLE SD MAILLAGE
            call jenuno(jexnum(main//'.GROUPENO', i), nom)
            call jelira(jexnom(main//'.GROUPENO', nom), 'LONUTI', nbnogr)
            call jeveuo(jexnom(main//'.GROUPENO', nom), 'L', jgnoi)
            AS_ALLOCATE(vi=noeud_group, size=nbnogr)
            kk=0
            do 60 j = 1, nbnogr
                if (zi(jnon+zi(jgnoi+j-1)-1) .eq. 0) then
                    kk=kk+1
                    noeud_group(kk)=zi(jgnoi+j-1)
                endif
60          continue
!           NOMBRE DE NOEUDS DU NOUVEAU GROUPE
            nbnogr=kk
!
!           ON AJOUTE DANS '.GROUPENO' LES NUMEROS DES NOEUDS
            if (nbnogr .ne. 0) then
                call jecroc(jexnom(grpno, nom))
                call jeecra(jexnom(grpno, nom), 'LONMAX', max(1,nbnogr))
                call jeecra(jexnom(grpno, nom), 'LONUTI', nbnogr)
                call jeveuo(jexnom(grpno, nom), 'E', jgnou)
                do 70 j = 1, nbnogr
                    ij=noeud_group(j)
                    call jenuno(jexnum(nomnoi, ij), nnoi)
                    call jenonu(jexnom(nomnoe, nnoi), zi(jgnou+j-1))
70              continue
            endif
!
            AS_DEALLOCATE(vi=noeud_group)
!
50      continue
!
!
    endif
    call jedetr('&&CMQLNO.NOM_NOEUDS')
    call jedetr('&&CMQLNO_NOEUD')
!
    call jedema()
!
end subroutine
