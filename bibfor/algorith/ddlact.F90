subroutine ddlact(nomres, numddl)
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
    implicit none
!
!***********************************************************************
!    P. RICHARD     DATE 19/02/91
!-----------------------------------------------------------------------
!  BUT:  GESTION DES DDL A PRENDRE EN COMPTE DANS LES EQUATIONS
!  DE CONTINUITE A L'INTERFACE, EN FONCTION DES DDL BLOQUE ET
!  DU TYPE D'INTERFACE
!
! UTILISATION D'UN TABLEAU VOLATIL DE DESCRIPTION DES DDL PORTES PAR
!   LES NOEUDS:
!
!  COLONNE 1: ENTIERS CODES DDL PHYSIQUE ASSEMBLES
!  COLONNE 2 : ENTIERS CODES LAGRANGE DE DUALISATION ASSEMBLES
!
!   REMARQUE:LES DDL DU IPRNO NE PORTE QUE DE DDL A CODE POSITIF CAR LE
!     IPRNO CORRESPOND AU PRNO DU LIGREL MAILLAGE -> DECODAGE SUR 6
!-----------------------------------------------------------------------
!
! NOMRES   /I/: NOM UTILISATEUR DES RESULTATS DE L'OPERATEUR
! NUMDDL   /I/: NOM DE LA NUMEROTATION CORRESPONDANT AU PB
!
!
!
!
!
#include "jeveux.h"
#include "asterfort/actiau.h"
#include "asterfort/acticb.h"
#include "asterfort/actimn.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/recddl.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=6) :: pgc
    character(len=8) :: nomres, typint
    character(len=19) :: numddl
    character(len=24) :: desdef, deeq, temmat, noeint, actint, temdec
    real(kind=8) :: actifs
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iec, ino, j, ldact, lldeeq
    integer :: lldes, llnoe, lltypi, ltcono, ltidec, ltmat, ltnono
    integer :: nbcmp, nbec, nbint, nbno, nbnot, neq, nomax
!
!-----------------------------------------------------------------------
    data pgc /'DDLACT'/
!-----------------------------------------------------------------------
!
    call jemarq()
    noeint=nomres//'.IDC_LINO'
    actint=nomres//'.IDC_DDAC'
!
!------RECUPERATION DONNEES RELATIVES A LA GRANDEUR SOUS-JACENTE--------
!            ET ALLOCATION VECTEUR TRAVAIL DECODAGE
!
    call dismoi('NB_CMP_MAX', nomres, 'INTERF_DYNA', repi=nbcmp)
    call dismoi('NB_EC', nomres, 'INTERF_DYNA', repi=nbec)
    temdec='&&'//pgc//'.IDEC'
    call wkvect(temdec, 'V V I', nbcmp*nbec*2, ltidec)
!
!-----------REQUETTE ADRESSE DE LA TABLE DESCRIPTION DES DEFORMEES------
!
    desdef=nomres//'.IDC_DEFO'
    call jeveuo(desdef, 'L', lldes)
    call jelira(desdef, 'LONMAX', nbnot)
    nbnot=nbnot/(2+nbec)
!
!---------------RECUPERATION DU NOMBRE D'INTERFACE----------------------
!
    call jelira(noeint, 'NMAXOC', nbint)
!
!---------------RECUPERATION DES TYPES D'INTERFACE ---------------------
!
    call jeveuo(nomres//'.IDC_TYPE', 'L', lltypi)
!
!----------------COMPTAGE DU NOMBRE MAX DE NOEUDS DES INTERFACE---------
!
    nomax=0
    do i = 1, nbint
        call jelira(jexnum(noeint, i), 'LONMAX', nbno)
        nomax=max(nomax,nbno)
    end do
!
!---------CREATION DU NOM DE LA MATRICE DESCRIPTIVE DES DDL-------------
!
    nomax=2*nomax
    temmat='&&'//pgc//'.MATDDL'
    call wkvect(temmat, 'V V I', nomax*nbec, ltmat)
!
!--------------------CREATION VECTEUR DE TRAVAIL------------------------
!
    call wkvect('&&'//pgc//'.NONO', 'V V I', nomax, ltnono)
    call wkvect('&&'//pgc//'.CONO', 'V V I', nomax*nbec, ltcono)
!
!--------------------REQUETE SUR LE DEEQ DU NUMDDL----------------------
!
    deeq=numddl//'.DEEQ'
    call jeveuo(deeq, 'L', lldeeq)
    call dismoi('NB_EQUA', numddl, 'NUME_DDL', repi=neq)
!
!-----------------------BOUCLE SUR LES INTERFACES-----------------------
!
    do i = 1, nbint
        call jelira(jexnum(noeint, i), 'LONMAX', nbno)
        call jeveuo(jexnum(noeint, i), 'L', llnoe)
        call jeveuo(jexnum(actint, i), 'E', ldact)
!
        do j = 1, nbno
            ino=zi(llnoe+j-1)
            zi(ltnono+j-1)=zi(lldes+ino-1)
            do iec = 1, nbec
                zi(ltcono+(j-1)*nbec+iec-1)= zi(lldes+2*nbnot+(ino-1)*&
                nbec+iec-1)
            end do
        end do
!
        call recddl(nbcmp, zi(ltnono), nbno, nbec, zi(lldeeq),&
                    neq, zi(ltmat), zi(ltidec))
!
        typint=zk8(lltypi+i-1)
!
        if (typint .eq. 'CRAIGB  ' .or. typint .eq. 'CB_HARMO') then
            call acticb(nbcmp, nbno, nbec, zi(ltmat), zi(ltcono),&
                        zi(ldact))
        endif
!
        if (typint .eq. 'MNEAL   ') then
            call actimn(nbcmp, nbno, nbec, zi(ltmat), zi(ldact))
        endif
!
        if (typint .eq. 'AUCUN   ') then
            call actiau(nbcmp, nbno, nbec, zi(ltmat), zi(ldact))
        endif
!
!--  TEST SUR LA PRESENCE DE DDL ACTIFS
!
        actifs=0.d0
        do j = 1, nbno*nbec
            actifs=actifs+zi(ldact+j-1)**2
        end do
!
        if (actifs .lt. 1) then
            call utmess('F', 'SOUSTRUC2_8')
        endif
!
        call jelibe(jexnum(actint, i))
        call jelibe(jexnum(noeint, i))
!
    end do
!
    call jelibe(deeq)
    call jedetr(temdec)
    call jedetr('&&'//pgc//'.NONO')
    call jedetr('&&'//pgc//'.CONO')
    call jedetr(temmat)
!
    call jedema()
end subroutine
