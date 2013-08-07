subroutine disexc(nindex, ilfex1, illex, npdsc3, iadsc3)
    implicit none
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
!  BUT: CALCULER LES INTERSPECTRES EXCITATION DANS LA DISCRETISATION
!       FREQUENTIELLE DE LA REPONSE
!        (CALCUL DYNAMIQUE ALEATOIRE)
!
!-----------------------------------------------------------------------
!
! NINDEX   /IN /: NOMBRE  D INDICES RECUPERES
! ILFEX1   /IN /: ADRESSE DANS ZI DE LA LISTE DES ADR DES .VALE
! ILLEX    /IN /: ADR INFO FREQ
! NPDSC3   /IN/: NOMBRE DE VALEURS DE FREQUENCE
! IADSC3   /IN/: POINTEUR DANS ZR DES VALEURS DE FREQUENCES
! ILFEX2   /OUT /: POINTEUR DANS ZI DES DEBUTS DE SEGMENTS ZR DES
!                 VALEURS R,I DES FONCTIONS INTERSPECTRALES  DE
!                 L EXCITATION RECALCULEES SUR DISCR REPONSE
!
#include "jeveux.h"
!
#include "asterfort/fointr.h"
#include "asterfort/jecreo.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveut.h"
#include "asterfort/wkvect.h"
    integer :: ij1, i1, i2, ireim1, ireim2
    integer :: ilong1
    character(len=8) :: chlist(5)
    character(len=24) :: k24bd1
!
!-----------------------------------------------------------------------
    integer :: iadsc3, ibid2, ilfex1, ilfex2
    integer :: illex, itail1, nindex, npdsc3
!
!-----------------------------------------------------------------------
    call jemarq()
    itail1=(nindex*(nindex+1))/2
    call wkvect('&&OP0131.LIADRFOE.FRQRE', 'V V I', itail1, ilfex2)
    do 403 i1 = 1, nindex
        do 404 i2 = i1, nindex
            write(k24bd1,'(A8,A3,2I4.4,A5)')'&&OP0131','.FO',i1,i2,&
            '.VALE'
            ij1=(i2*(i2-1))/2+i1
            call jecreo(k24bd1, 'V V R8')
            call jeecra(k24bd1, 'LONMAX', npdsc3*3)
            call jeecra(k24bd1, 'LONUTI', npdsc3*3)
            call jeveut(k24bd1, 'E', zi(ilfex2-1+ij1))
404      continue
403  end do
    call wkvect('&&OP0131.REIM.ARRIVE', 'V V R8', 2*npdsc3, ireim2)
    do 411 i1 = 1, itail1
        ilong1=zi(illex)
        call wkvect('&&OP0131.REIM.DEPART', 'V V R8', 2*ilong1, ireim1)
        do 428 i2 = 1, ilong1
            if (ilong1 .eq. zi(illex+i1)) then
                zr(ireim1-1+i2)=zr(zi(ilfex1+i1-1)-1+i2)
                zr(ireim1+ilong1-1+i2)=0.d0
            else
                zr(ireim1-1+i2)=zr(zi(ilfex1+i1-1)-1+2*(i2-1)+1)
                zr(ireim1+ilong1-1+i2)=zr(zi(ilfex1+i1-1)-1+ 2*(i2-1)+&
                2)
            endif
428      continue
        chlist(1)='FONCTION'
        chlist(2)='LIN LIN '
        chlist(3)='TOTO'
        chlist(4)='TOTO'
        chlist(5)='CC      '
        call fointr(' ', chlist(1), ilong1, zr(zi(illex+itail1+1)), zr(ireim1),&
                    npdsc3, zr(iadsc3), zr(ireim2), ibid2)
        call fointr(' ', chlist(1), ilong1, zr(zi(illex+itail1+1)), zr(ireim1+ilong1),&
                    npdsc3, zr(iadsc3), zr(ireim2+npdsc3), ibid2)
        do 429 i2 = 1, npdsc3
            zr(zi(ilfex2+i1-1)+npdsc3-1+2*(i2-1)+1)=zr(ireim2-1+i2)
            zr(zi(ilfex2+i1-1)+npdsc3-1+2*(i2-1)+2)= zr(ireim2+npdsc3-&
            1+i2)
429      continue
        call jedetr('&&OP0131.REIM.DEPART')
411  end do
    call jedetr('&&OP0131.REIM.ARRIVE')
    call jedema()
end subroutine
