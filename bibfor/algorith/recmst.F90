subroutine recmst(graexc, grdmod, nnoeex, ilnoex, ilcpex,&
                  nmost1, modsta)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!***********************************************************************
!    C. DUVAL
!-----------------------------------------------------------------------
!  BUT: RECUPERER LES INFORMATIONS DE TYPE MODE STATIQUE POUR
    implicit none
!        LE CALCUL DYNAMIQUE ALEATOIRE
!
!-----------------------------------------------------------------------
!
! GRAEXC   /IN / : GRANDEUR EXCITATION
! GRDMOD   /IN / : GRANDEUR A RECUPERE DANS LES MODES DYN ET STAT
! NNOEEX   /IN / : NOMBRE DE NOEUDS EXCITATION
! ILNOEX   /IN / : POINTEUR DANS ZK8 SUR LES NOEUDS EXCITATION
! ILCPEX   /IN / : POINTEUR DANS ZK8 SUR LES DDLS EXCITATION
! NMOST1   /OUT/ : NOMBRE D OCCURENCE DU MOT CLE MODE_STAT
! MODSTA   /OUT/ : CONCEPT MODE_STAT
!                     AUX MODES STATIQUES EN DEPLACEMENT
!                     AUX MODES STATIQUES EN GRANDEUR REPONSE (CALCUL)
!
!
!
!
!
#include "jeveux.h"
#include "asterfort/getvid.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jeveut.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/rsadpa.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: kbid, modsta
    character(len=16) :: graexc, grdmod
    character(len=24) :: k24bd1, k24bd2, k24bd3
!
!---------MODES STATIQUES
!
!-----------------------------------------------------------------------
    integer :: i1, i2, i3, i3b, i4, ibid, ilamsc
    integer :: ilamst, ilcpex, ilnoex, ilorms, jordr, jpara, n
    integer :: nmost1, nmost2, nnoeex
!-----------------------------------------------------------------------
    call jemarq()
    call getvid(' ', 'MODE_STAT', nbval=0, nbret=n)
    nmost1=-n
    if (n .ne. 0) then
        call getvid(' ', 'MODE_STAT', scal=modsta, nbret=ibid)
    endif
!
!---------CONSTITUTION DE LA LISTE DES ADRESSES DES MODES STATIQUES
!
    if (graexc .ne. 'DEPL_R') goto 9999
!
    k24bd1=modsta//'           .NOEU'
!     CALL JELIRA(K24BD1,'LONMAX',NMOST2,K24BD2)
!     CALL JEVEUO(K24BD1,'L',IAD1)
    call jelira(modsta//'           .ORDR', 'LONMAX', nmost2)
    call jeveuo(modsta//'           .ORDR', 'L', jordr)
    call wkvect('&&OP0131.LISTADORMOSTA', 'V V I', nnoeex, ilorms)
    do 212,i1=1,nnoeex
    zi(ilorms+i1-1)=0
    do 213,i2=1,nmost2
    call rsadpa(modsta, 'L', 1, 'NOEUD_CMP', zi(jordr-1+i2),&
                0, sjv=jpara, styp=kbid)
    if ((zk8(ilnoex+i1-1)//zk8(ilcpex+i1-1)) .eq. zk16(jpara)) then
        zi(ilorms+i1-1)=i2
    endif
213  continue
    212 end do
!
!---------CONSTITUTION DES ADRESSES DES MODES STATIQUES
!
    call wkvect('&&OP0131.LISTADRMODSTA', 'V V I', nnoeex, ilamst)
    call wkvect('&&OP0131.LISTADRMODSTAC', 'V V I', nnoeex, ilamsc)
    do 214,i1=1,nnoeex
    i2=zi(ilorms+i1-1)
    if (i2 .eq. 0) then
        call utmess('F', 'ALGORITH10_33')
    endif
    k24bd1=modsta//'           .TACH'
    call jenonu(jexnom(k24bd1(1:19)//'.DESC', 'DEPL'), ibid)
    call jeveuo(jexnum(k24bd1, ibid), 'L', i3)
    call jenonu(jexnom(k24bd1(1:19)//'.DESC', grdmod), ibid)
    call jeveuo(jexnum(k24bd1, ibid), 'L', i3b)
    k24bd2=zk24(i3+i2-1)
    k24bd3=k24bd2(1:19)//'.VALE'
    call jeveut(k24bd3, 'L', i4)
    zi(ilamst+i1-1)=i4
    k24bd2=zk24(i3b+i2-1)
    k24bd3=k24bd2(1:19)//'.VALE'
    call jeveut(k24bd3, 'L', i4)
    zi(ilamsc+i1-1)=i4
    214 end do
9999  continue
    call jedema()
end subroutine
