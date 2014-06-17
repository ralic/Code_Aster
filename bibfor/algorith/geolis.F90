subroutine geolis(modgen, sst1, sst2, intf1, intf2,&
                  geom1, geom2, limail, nmga1)
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
    implicit none
!***********************************************************************
!  O. NICOLAS     DATE 01/08/04
!-----------------------------------------------------------------------
!  BUT:      < CALCULER DES TRANSFORMATIONS GEOMETRIQUES >
!
!  CALCUL DES TRANSFORMATIONS GEOMETRIQUES DES SOUS STRUCTURES DANS LE
!  CAS DES INTERFACES INCOMPATIBLES
!
!-----------------------------------------------------------------------
!
! MODGEN  /I/ : NOM K8 DU MODELE GENERALISE
! SST1    /I/ : NOM K8 DE LA PREMIERE SOUS-STRUCTURE DE LA LIAISON
! SST2    /I/ : NOM K8 DE LA SECONDE SOUS-STRUCTURE DE LA LIAISON
! INTF1   /I/ : NOM K8 DE L'INTERFACE DE SST1
! INTF2   /I/ : NOM K8 DE L'INTERFACE DE SST2
! GEOM1   /O/ : NOM K8 DE LA TRANSFORMATION GEOMETRIQUE DE LA SS 1
! GEOM2   /O/ : NOM K8 DE LA TRANSFORMATION GEOMETRIQUE DE LA SS 2
! LIMAIL  /I/ : NOM DU GROUPE DE MAILLE MAITRE
! NMGA1  /I/ : NOMBRE DE MAILLES DANS L'INTERFACE MAITRE
!
!
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterfort/dismoi.h"
#include "asterfort/gmgnre.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/matrot.h"
#include "asterfort/mgutdm.h"
#include "asterfort/parotr.h"
#include "asterfort/wkvect.h"
!
!
!
!
    integer :: iageo2, iageo1, igeom2, nbno2, ino2, nuno2, nno2, igeom1, nbno1
    integer :: ino1, nuno1, nno1, iagma1, nmga1, nma1, i, k, kk, ibid
    integer :: nusst1, nusst2, llrot1, llrot2, lltra1, lltra2, llint2
    integer :: ialino
    real(kind=8) :: tra1(3), angl1(3), centr1(3), tra2(3), angl2(3), centr2(3)
    real(kind=8) :: coor1(3), coor2(3), zero, un, rot1(3, 3), rot2(3, 3)
    character(len=8) :: modgen, lint2, sst1, sst2, intf1, intf2, mail1, mail2
    character(len=24) :: repnom, int2, toto, geom2, geom1
    character(len=*) :: limail
    integer, pointer :: idc_defo(:) => null()
!
!-----------------------------------------------------------------------
!
    call jemarq()
! --- INITIALISATIONS :
!     ---------------
    zero = 0.0d0
    un = 1.0d0
!
    do k = 1, 3
!
        tra1(k) = zero
        tra2(k) = zero
        angl1(k)= zero
        angl2(k)= zero
        centr1(k) = zero
        centr2(k) = zero
!
        do kk = 1, 3
            if (k .eq. kk) then
                rot1(k,k) = un
                rot2(k,k) = un
            else
                rot1(k,kk) = zero
                rot1(kk,k) = zero
                rot2(k,kk) = zero
                rot2(kk,k) = zero
            endif
        end do
    end do
!
!
!
!-----RECUPERATION DES ROTATIONS ET DES TRANSLATIONS
!
    repnom=modgen//'      .MODG.SSNO'
    call jenonu(jexnom(repnom, sst1), nusst1)
    call jenonu(jexnom(repnom, sst2), nusst2)
    call jeveuo(jexnum(modgen//'      .MODG.SSOR', nusst1), 'L', llrot1)
    call jeveuo(jexnum(modgen//'      .MODG.SSOR', nusst2), 'L', llrot2)
    do i = 1, 3
        angl1(i)=zr(llrot1+i-1)*r8dgrd()
        angl2(i)=zr(llrot2+i-1)*r8dgrd()
    end do
    call jeveuo(jexnum(modgen//'      .MODG.SSTR', nusst1), 'L', lltra1)
    call jeveuo(jexnum(modgen//'      .MODG.SSTR', nusst2), 'L', lltra2)
    do i = 1, 3
        tra1(i)=zr(lltra1+i-1)
        tra2(i)=zr(lltra2+i-1)
    end do
!
    call matrot(angl1, rot1)
    call matrot(angl2, rot2)
!
!-----RECUPERATION MAILLAGE
!
    call mgutdm(modgen, sst1, ibid, 'NOM_MAILLAGE', ibid,&
                mail1)
    call mgutdm(modgen, sst2, ibid, 'NOM_MAILLAGE', ibid,&
                mail2)
!
!
! --- DETERMINATION DES COORDONNEES TRANSFORMEES MAITRE :
!     --------------------------------------------------
    call dismoi('NB_MA_MAILLA', mail1, 'MAILLAGE', repi=nma1)
    call dismoi('NB_NO_MAILLA', mail1, 'MAILLAGE', repi=nno1)
    call wkvect(geom1, 'V V R', 3*nno1, igeom1)
    call jeveuo(mail1//'.COORDO    .VALE', 'L', iageo1)
!
    call jeveuo(limail, 'L', iagma1)
!
    toto='TUTU'
    call wkvect(toto, 'V V I', 2*nno1, ialino)
!
    call gmgnre(mail1, nno1, zi(ialino), zi(iagma1), nmga1,&
                zi(ialino+nno1), nbno1, 'TOUS')
!
    do ino1 = 1, nbno1
        nuno1 = zi(ialino+nno1+ino1-1)
        call parotr(mail1, iageo1, nuno1, 0, centr1,&
                    rot1, tra1, coor1)
        do k = 1, 3
            zr(igeom1+3*(nuno1-1)+k-1) = coor1(k)
        end do
    end do
!
! --- DETERMINATION DES COORDONNEES TRANSFORMEES ESCLAVE :
!     ---------------------------------------------------
    call mgutdm(modgen, sst2, ibid, 'NOM_LIST_INTERF', ibid,&
                lint2)
    int2=lint2//'.IDC_LINO'
    call jenonu(jexnom(int2(1:13)//'NOMS', intf2), ibid)
    call jelira(jexnum(int2, ibid), 'LONMAX', nbno2)
    call jeveuo(jexnum(lint2 //'.IDC_LINO', ibid), 'L', llint2)
    call jeveuo(lint2//'.IDC_DEFO', 'L', vi=idc_defo)
!
    call dismoi('NB_NO_MAILLA', mail2, 'MAILLAGE', repi=nno2)
    call wkvect(geom2, 'V V R', 3*nno2, igeom2)
    call jeveuo(mail2//'.COORDO    .VALE', 'L', iageo2)
!
!     Recuperation des numeros des noeuds esclaves
    call jenonu(jexnom(lint2 //'.IDC_NOMS', intf2), ibid)
!
    do ino2 = 1, nbno2
        nuno2 = idc_defo(ino2)
        call parotr(mail2, iageo2, nuno2, 0, centr2,&
                    rot2, tra2, coor2)
        do k = 1, 3
            zr(igeom2+3*(nuno2-1)+k-1) = coor2(k)
        end do
    end do
!
    call jedetr(toto)
    call jedema()
end subroutine
