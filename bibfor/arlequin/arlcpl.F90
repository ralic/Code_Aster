subroutine arlcpl(zocc ,nbma1 ,nbma2 , &
                  mail  ,nomo  ,typmai,        &
                  nom1  ,nom2  ,ndim, lisrel, charge)

! ======================================================================
! COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================

    implicit none

#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "asterfort/arlmai.h"
#include "asterfort/arlmod.h"
#include "asterfort/arlcp2.h"
#include "asterfort/codent.h"
#include "asterfort/jeexin.h"
#include "asterfort/jexnum.h"
#include "asterfort/jeveuo.h"
#include "asterfort/arlcp3.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedema.h"

    character(len=24) :: typmai
    character(len=8)  :: mail,nomo
    character(len=8) ::  charge
    character(len=19) :: lisrel
    character(len=10) :: nom1,nom2
    integer           :: ndim,zocc
    integer           :: nbma1,nbma2

! ----------------------------------------------------------------------

! ROUTINE ARLEQUIN

! CALCUL DES MATRICES DE COUPLAGE ELEMENTAIRE ARLEQUIN
! ASSEMBLAGE DANS LES MATRICES ARLEQUIN MORSES

! ----------------------------------------------------------------------

! IN  MAIL   : NOM DU MAILLAGE
! IN  NOMO   : NOM DU MODELE
! IN  TYPMAI : SD CONTENANT NOM DES TYPES ELEMENTS (&&CATA.NOMTM)
! IN  NOM1   : NOM DE LA SD DE STOCKAGE PREMIER GROUPE
! IN  NOM2   : NOM DE LA SD DE STOCKAGE SECOND GROUPE
! IN  NDIM   : DIMENSION DE L'ESPACE GLOBAL (2 OU 3)


    integer :: nbnomx
    parameter    (nbnomx=27)
    integer          ::  nliai,nddl
    parameter    (nliai=12,nddl=nliai*nliai)
    logical(kind=1)          ::  proj
    character(len=8) ::  marlel,modarl,mailar
    character(len=24)::  tabcor
    integer          ::  iaux,jaux
    integer          ::  imatu1,imatu2,iexi
    integer          ::  jma1,jma2
    integer          ::  nbnoc1,nbnoc2
    integer          ::  chtest,i,j,jj,k
    character(len=19)::  ligarl,arlmt1,arlmt2
    real(kind=8)     ::  m1de(nliai,nliai)
    real(kind=8)     ::  m3de(nliai,3*nbnomx)
    character(len=5) ::  ch1,ch2

    integer          ::  len1,len2, iproj
    character(len=5), dimension(2+nbnomx,nbma1) :: numno1
    character(len=5), dimension(2,nbma2)  :: numno2
    character(len=5), dimension(nbnomx*nbma1) :: numn1t
    character(len=5), dimension(2*nbma2)  :: numn2t
    real(kind=8)     ::  m3dea(12,3*nbnomx,nbma1),m1dea(12,12,nbma2)

! ----------------------------------------------------------------------
    call jemarq()

! --- INITIALISATIONS

    numno1 = '0'
    numno2 = '0'
    numn1t = '0'
    numn2t = '0'
    m1dea = 0.0
    m3dea = 0.0

! --------------------------------------------------------------------

    marlel = '&&MARLEL'

! --- CREATION PSEUDO-MAILLAGE

    call arlmai(mail,mailar,ndim,nom1,nom2, &
                tabcor,nbma1,nbma2)

! --- CREATION PSEUDO-MODELE

    call arlmod(nomo,mailar,modarl,tabcor)

! --- MATRICES DE COUPLAGE ELEMENTAIRES

    iproj = 0
    proj = .false.
    do 777 jma2 = 1, nbma2
        do 888 jma1 = 1, nbma1
            call arlcp2(zocc,mail,nomo,typmai, &
                        nom1,nom2,marlel,modarl, &
                        jma1,jma2,tabcor,mailar,proj)
            if (proj) then
                iproj = iproj + 1
                ligarl = modarl(1:8)//'.MODELE'
                arlmt2 = marlel(1:8)//'.ARLMT2'
                call jeexin(jexnum(arlmt2(1:19)//'.RESL',2),iexi)
                if (iexi == 0) goto 210
                call jeveuo(jexnum(arlmt2(1:19)//'.RESL',2),'L',imatu2)
                nbnoc1 = nint(zr(imatu2+nddl))
                nbnoc2 = nint(zr(imatu2+nddl+1))
                do 10 j = 1,nbnoc2
                    call codent(nint(zr(imatu2-1+(nddl+4)+nbnoc1+j)),'G',ch2)
                    numno2(j,jma2) = ch2
                    numno1(j,iproj) = ch2
                10 end do
                do 20 j = 1,nbnoc1
                    call codent(nint(zr(imatu2-1+(nddl+4)+j)),'G',ch1)
                    numno1(nbnoc2+j,iproj) = ch1
                20 end do

! --- RECUPERATION DE LA MATRICE DE COUPLAGE 1D-1D

                do 160 iaux = 1,6*nbnoc2
                    do 150 jaux = 1,6*nbnoc2
                        m1de(iaux,jaux) = zr(imatu2-1+(6*nbnoc2)*(iaux-1)+jaux)
                        m1dea(iaux,jaux,jma2) = m1de(iaux,jaux)
                    150 end do
                160 end do

! --- RECUPERATION DE LA MATRICE DE COUPLAGE 1D-3D            

                arlmt1 = marlel(1:8)//'.ARLMT1'
                call jeexin(jexnum(arlmt1(1:19)//'.RESL',1),iexi)
                if (iexi == 0) goto 210
                call jeveuo(jexnum(arlmt1(1:19)//'.RESL',1),'L',imatu1)
                do 140 iaux = 1,6*nbnoc2
                    do 130 jaux = 1,3*nbnoc1
                        m3de(iaux,jaux) = zr(imatu1-1+(3*nbnoc1)*(iaux-1)+jaux)
                        m3dea(iaux,jaux,iproj) = m3de(iaux,jaux)
                    130 end do
                140 end do
            endif
        888 end do
    777 end do

! --- CREATION DES VECTEURS NUMEROS NOEUDS POUR L'ASSEMBLAGE

    jj=0
    do 71 i=1,nbma2
        do 72 j=1,nbnoc2
            chtest = 0
            do 73 k=1,nbnoc2*nbma2
                if (numno2(j,i) == numn2t(k)) then
                    chtest = 1
                endif
            73 end do
            if (chtest == 0) then
                jj=jj+1
                numn2t(jj) = numno2(j,i)
            endif
        72 end do
    71 end do
    len2=jj
    jj=0
    do 76 i=1,nbma1
        do 77 j=1,nbnoc1
            chtest = 0
            do 78 k=1,nbnoc1*nbma1
                if ((numno1(2+j,i) == numn1t(k)) .or. (numno1(2+j,i) == '0')) then
                    chtest = 1
                endif
            78 end do
            if (chtest == 0) then
                jj=jj+1
                numn1t(jj) = numno1(2+j,i)
            endif
        77 end do
    76 end do
    len1=jj

! --- ASSEMBLAGE DES MATRICES DE COUPLAGE ELEMENTAIRES
! --- ET AFFECTATION DES RELATIONS CINEMATIQUES

    call arlcp3(nbma1,nbma2,numno1,numno2,m3dea , &
                m1dea,numn1t,numn2t,len1,len2,lisrel,charge)

    210 continue

    call jedetr(tabcor)

    call jedema()

end subroutine

