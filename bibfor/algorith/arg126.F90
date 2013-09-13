subroutine arg126(nomres)
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
    implicit none
!  P. RICHARD     DATE 13/10/93
!-----------------------------------------------------------------------
!  BUT:      < RECUPERATION DES ARGUMENTS POUR OP0126 >
!
!  RECUPERER LES ARGUMENTS UTILISATEUR POUR LA DEFINITION DU MODELE
!  GENERALISE. DEFINITION DES SOUS-STRUCTURES ET DES LIAISONS ENTRE
!  LES SOUS-STRUCTURES.
!
!-----------------------------------------------------------------------
!
! NOMRES   /I/: NOM UTILISATEUR DU RESULTAT
!
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/mgutdm.h"
#include "asterfort/utmess.h"
!
    integer :: vali(3)
!
!
!
    character(len=8) :: nomres, lintf, nomsst, mclcou, nomcou
    character(len=16) :: clesst, clenom, clerot, clemcl, cletra, clelia, clel(4)
    character(len=24) :: repsst, nommcl, rotsst, famli, trasst
    character(len=24) :: valk(3)
    integer :: nbsst, i, j, ioc, ibid, ldnmcl, ldrot, nblia, ldlid, iret, ldtra
    real(kind=8) :: pi
!
!-----------------------------------------------------------------------
    data clesst,clenom /'SOUS_STRUC','NOM'/
    data clerot,clemcl /'ANGL_NAUT','MACR_ELEM_DYNA'/
    data clelia,cletra /'LIAISON','TRANS'/
    data clel          /'SOUS_STRUC_1','SOUS_STRUC_2','INTERFACE_1',&
     &                    'INTERFACE_2'/
!-----------------------------------------------------------------------
!
    call jemarq()
    pi=4.d+00*atan2(1.d+00,1.d+00)
!
!-----TRAITEMENT DEFINITION SOUS-STRUCTURES-----------------------------
!
    call getfac(clesst, nbsst)
!
    if (nbsst .lt. 2) then
        vali (1) = 2
        vali (2) = nbsst
        call utmess('F', 'ALGORITH11_92', ni=2, vali=vali)
    endif
!
    repsst=nomres//'      .MODG.SSNO'
    nommcl=nomres//'      .MODG.SSME'
    rotsst=nomres//'      .MODG.SSOR'
    trasst=nomres//'      .MODG.SSTR'
!
    call jecreo(repsst, 'G N K8')
    call jeecra(repsst, 'NOMMAX', nbsst)
    call jecrec(nommcl, 'G V K8', 'NU', 'CONTIG', 'CONSTANT',&
                nbsst)
    call jecrec(rotsst, 'G V R', 'NU', 'CONTIG', 'CONSTANT',&
                nbsst)
    call jecrec(trasst, 'G V R', 'NU', 'CONTIG', 'CONSTANT',&
                nbsst)
    do 300 i = 1, nbsst
        call jecroc(jexnum(nommcl, i))
        call jecroc(jexnum(rotsst, i))
        call jecroc(jexnum(trasst, i))
300  end do
    call jeecra(nommcl, 'LONT', nbsst)
    call jeecra(rotsst, 'LONT', 3*nbsst)
    call jeecra(trasst, 'LONT', 3*nbsst)
!
!
!-----BOUCLE SUR LES SOUS-STRUCTURES DEFINIES-------------------------
!
    do 10 i = 1, nbsst
        call getvtx(clesst, clenom, iocc=i, nbval=0, nbret=ioc)
        ioc=-ioc
        if (ioc .ne. 1) then
            vali (1) = 1
            vali (2) = ioc
            call utmess('F', 'ALGORITH11_93', ni=2, vali=vali)
        else
            call getvtx(clesst, clenom, iocc=i, scal=nomsst, nbret=ibid)
        endif
        call jecroc(jexnom(repsst, nomsst))
!
        call getvid(clesst, clemcl, iocc=i, nbval=0, nbret=ioc)
        ioc=-ioc
        if (ioc .ne. 1) then
            valk (1) = nomsst
            vali (1) = ioc
            vali (2) = 1
            call utmess('F', 'ALGORITH11_94', sk=valk(1), ni=2, vali=vali)
        else
            call getvid(clesst, clemcl, iocc=i, scal=mclcou, nbret=ibid)
        endif
        call jenonu(jexnom(repsst, nomsst), ibid)
        call jeveuo(jexnum(nommcl, ibid), 'E', ldnmcl)
        zk8(ldnmcl)=mclcou
!
!  TRAITEMENT DES ROTATIONS
!
        call jenonu(jexnom(repsst, nomsst), ibid)
        call jeveuo(jexnum(rotsst, ibid), 'E', ldrot)
        call getvr8(clesst, clerot, iocc=i, nbval=0, nbret=ioc)
        ioc=-ioc
        if (ioc .eq. 0) then
            do 30 j = 1, 3
                zr(ldrot+j-1)=0.d+00
30          continue
        else if (ioc.eq.3) then
            call getvr8(clesst, clerot, iocc=i, nbval=3, vect=zr(ldrot),&
                        nbret=ibid)
            do 20 j = 1, 3
                zr(ldrot+j-1)=zr(ldrot+j-1)*pi/180.d+00
20          continue
        else
            valk (1) = nomsst
            vali (1) = ioc
            vali (2) = 3
            call utmess('F', 'ALGORITH11_95', sk=valk(1), ni=2, vali=vali)
        endif
!
!  TRAITEMENT DES TRANSLATIONS SI INTRODUIT PAR L'UTILISATEUR
!
!
        call jenonu(jexnom(repsst, nomsst), ibid)
        call jeveuo(jexnum(trasst, ibid), 'E', ldtra)
        call getvr8(clesst, cletra, iocc=i, nbval=0, nbret=ioc)
        ioc=-ioc
        if (ioc .eq. 0) then
            do 40 j = 1, 3
                zr(ldtra+j-1)=0.d+00
40          continue
        else if (ioc.eq.3) then
            call getvr8(clesst, cletra, iocc=i, nbval=3, vect=zr(ldtra),&
                        nbret=ibid)
        else
            valk (1) = nomsst
            vali (1) = ioc
            vali (2) = 3
            call utmess('F', 'ALGORITH11_96', sk=valk(1), ni=2, vali=vali)
        endif
!
!
10  end do
!
!-----RECUPERATION DU NOMBRE DE LIAISONS DEFINIES-----------------------
!
    call getfac(clelia, nblia)
    if (nblia .eq. 0) then
        vali (1) = nblia
        vali (2) = 1
        call utmess('F', 'ALGORITH11_97', ni=2, vali=vali)
    endif
!
    famli=nomres//'      .MODG.LIDF'
    call jecrec(famli, 'G V K8', 'NU', 'DISPERSE', 'CONSTANT',&
                nblia)
    call jeecra(famli, 'LONMAX', 5)
!
!-----BOUCLE SUR LES LIAISONS------------------------------------------
!
    do 140 i = 1, nblia
        call jecroc(jexnum(famli, i))
        call jeveuo(jexnum(famli, i), 'E', ldlid)
!
!  BOUCLE SUR LES SOUS-STRUCTURES DE LA LIAISON
!
        do 150 j = 1, 2
            call getvtx(clelia, clel(j), iocc=i, nbval=0, nbret=ioc)
            ioc=-ioc
            if (ioc .ne. 1) then
                vali (1) = i
                vali (2) = ioc
                vali (3) = 1
                valk (1) = clel(j)
                call utmess('F', 'ALGORITH11_98', sk=valk(1), ni=3, vali=vali)
            else
                call getvtx(clelia, clel(j), iocc=i, scal=nomcou, nbret=ibid)
!
!  VERIFICATION EXISTANCE DE LA SOUS-STRUCTURE
!
                call jenonu(jexnom(repsst, nomcou), iret)
                if (iret .eq. 0) then
                    vali (1) = i
                    valk (1) = nomcou
                    call utmess('F', 'ALGORITH11_99', sk=valk(1), si=vali(1))
                endif
                zk8(ldlid+(j-1)*2)=nomcou
            endif
150      continue
!
!  BOUCLE SUR LES INTERFACES
!
        do 160 j = 3, 4
            call getvtx(clelia, clel(j), iocc=i, nbval=0, nbret=ioc)
            ioc=-ioc
            if (ioc .ne. 1) then
                vali (1) = i
                vali (2) = ioc
                vali (3) = 1
                valk (1) = clel(j)
                call utmess('F', 'ALGORITH11_98', sk=valk(1), ni=3, vali=vali)
            else
                call getvtx(clelia, clel(j), iocc=i, scal=nomcou, nbret=ibid)
            endif
!
!  VERIFICATION DE L'EXISTANCE DE L'INTERFACE
!
            nomsst=zk8(ldlid+(j-3)*2)
            call mgutdm(nomres, nomsst, ibid, 'NOM_LIST_INTERF', ibid,&
                        lintf)
            if (lintf(1:2) .eq. ' ') then
                call utmess('F', 'ALGORITH12_3', sk=nomsst)
            endif
            call jenonu(jexnom(lintf//'.IDC_NOMS', nomcou), iret)
            if (iret .eq. 0) then
                vali (1) = i
                valk (1) = nomsst
                valk (2) = '   '
                valk (3) = nomcou
                call utmess('F', 'ALGORITH12_2', nk=3, valk=valk, si=vali(1))
            endif
            zk8(ldlid+(j-3)*2+1)=nomcou
160      continue
!  ON INITIALISE L'ORDONANCEMENT A NON
        zk8(ldlid+4)='NON'
140  end do
!
!
!
    call jedema()
end subroutine
