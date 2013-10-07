subroutine op5901(nboccm, ifm, niv, compor)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jean-michel.proix at edf.fr
!     COMMANDE:  DEFI_COMPOR MONOCRISTAL
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/codent.h"
#include "asterfort/detrsd.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetc.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lcmmsg.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/tbexlr.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "blas/dcopy.h"
!
    character(len=8) :: compor, materi, typpar(5), chaine
    character(len=16) :: nompar(5), ecoule, ecrois, ecroci, elasti, nomvar(200)
    character(len=16) :: fasygl, noms(6), comdes, rota, tbinte, systgl
    character(len=19) :: listr
    real(kind=8) :: ms(6), ng(3), q(3, 3), lg(3), pgl(3, 3)
    complex(kind=8) :: cbid
    integer :: ifm, niv, nbtbsg, nums(2), indvar
    integer :: iocc, nbmat, nbecou, nbecro, nbcine, nbelas, nbfasy
    integer :: i, j, nbela1, nbsys, nvi, imk, imi, ipr, itab, itsg, irra, irr2
    integer :: ncprr, ir, irota, iadlr, decal, nbrota, nbsyst, tabdes(13)
    integer :: nboccm
!
!
!
    call jemarq()
!
    cbid=(0.d0,0.d0)
    comdes='&&OP0059.TABLETX'
    call tbcrsd(comdes, 'V')
    nompar(1)='FAMI_SYST_GLIS'
    nompar(2)='MAT_SYST'
    nompar(3)='ECOULEMENT'
    nompar(4)='ECRO_ISOT'
    nompar(5)='ECRO_CINE'
    typpar(1)='K16'
    typpar(2)='K16'
    typpar(3)='K16'
    typpar(4)='K16'
    typpar(5)='K16'
    nbsyst=0
    nbelas=0
    nvi=6
!     DEFORMATION PLASTIQUE CUMULEE MACROSCOPIQUE EQUIVALENTE
    nvi=nvi+1
    call tbajpa(comdes, 5, nompar, typpar)
    call getfac('MONOCRISTAL', nboccm)
    call wkvect(compor//'.CPRK', 'G V K24', 5*nboccm+1, imk)
!     DIMENSION MAX DE CPRR : 1812 = 12+5*6*30+30*30
!     ORGANISATION DE CPRR :
!     1 : NB TABLES SYST GLIS
!     2 : POSITION DE LA TABLE D'INTERACTION. 0 SINON
!     3 : NOMBRE DE SYSTEMES TABLE  1
!     4 : POSITION DE LA TABLE  1,
!     5 : NOMBRE DE SYSTEMES TABLE 2
!     6 : POSITION DE LA TABLE  2
!     ...
!     CPRR(4)   : TABLE DE SYS GLIS1 (6*NSYS1 VALEURS)
!     CPRR(6)    : TABLE DE SYS GLIS2 (6*NSYS2 VALEURS)
!     ...
!     CPRR(2)    : TABLE D'INTERACTION (NBSYST*NBSYST VALEURS)
    ncprr=1812
    call wkvect(compor//'.CPRR', 'G V R', ncprr, ipr)
    nbtbsg=0
    decal=12
    do 101 i = 1, 13
        tabdes(i)=0
101  end do
    irra=0
    irr2=0
!
    do 9 iocc = 1, nboccm
        call getvid('MONOCRISTAL', 'MATER', iocc=iocc, scal=materi, nbret=nbmat)
        call getvtx('MONOCRISTAL', 'ECOULEMENT', iocc=iocc, scal=ecoule, nbret=nbecou)
        call getvtx('MONOCRISTAL', 'ECRO_ISOT', iocc=iocc, scal=ecrois, nbret=nbecro)
        call getvtx('MONOCRISTAL', 'ECRO_CINE', iocc=iocc, scal=ecroci, nbret=nbcine)
        call getvtx('MONOCRISTAL', 'ELAS', iocc=iocc, scal=elasti, nbret=nbela1)
        if (nbela1 .gt. 0) then
            if (nbelas .eq. 0) then
                nbelas=1
            else
                call utmess('F', 'MODELISA5_64')
            endif
        endif
!        CAS DES LOIS DD
        if (ecoule(1:7) .eq. 'MONO_DD') then
            ecrois=ecoule
            ecroci=' '
        endif
!        CAS DES LOIS DD_CC_IRRA
        if (ecoule .eq. 'MONO_DD_CC_IRRA') then
            irra=irra+1
        endif
!        CAS DES LOIS DD_CFC_IRRA
        if (ecoule .eq. 'MONO_DD_CFC_IRRA') then
            irr2=irr2+1
        endif
!
        call getvtx('MONOCRISTAL', 'FAMI_SYST_GLIS', iocc=iocc, scal=fasygl, nbret=nbfasy)
        noms(1)=fasygl
        noms(2)=materi
        noms(3)=ecoule
        noms(4)=ecrois
        noms(5)=ecroci
        if (fasygl .eq. 'UTILISATEUR') then
            call getvid('MONOCRISTAL', 'TABL_SYST_GLIS', iocc=iocc, scal=systgl, nbret=itsg)
            noms(1)='UTIL'
            nbtbsg=nbtbsg+1
            call codent(nbtbsg, 'G', noms(1)(5:5))
            noms(1)(6:8)='___'
            noms(1)(9:16)=systgl(1:8)
            fasygl=noms(1)
            listr = '&&LCMMAT.TABL_SYSGL'
            call tbexlr(systgl, listr, 'V')
            call jeveuo(listr//'.VALE', 'L', iadlr)
            nbsys=nint(zr(iadlr+2))
!           VERIF QUE LA MATRICE EST CARREE
            if (6 .ne. zr(iadlr+1)) then
                call utmess('F', 'COMPOR2_19', sr=zr(iadlr+1))
            endif
            zr(ipr+2+2*(nbtbsg-1)) =nbsys
            zr(ipr+2+2*(nbtbsg-1)+1)=decal+1
            call dcopy(6*nbsys, zr(iadlr+3), 1, zr(ipr+decal), 1)
            tabdes(8+iocc)=nbsys
            call detrsd('LISTR8', listr)
!
            if (niv .eq. 2) then
                write(ifm,*) ' TABLE SYSTEMES DE GLISSEMENT FAMILLE',&
                iocc
                write(ifm,*) ' NX     NY     NZ     MX     MY     MZ '
                do 4 i = 1, nbsys
                    write(ifm,'(I2,6(1X,E11.4))') i,(zr(ipr-1+decal+6*&
                    (i-1)+j),j=1,6)
 4              continue
            endif
!
            decal=decal+6*nbsys
!
        else
            ir=0
            call lcmmsg(fasygl, nbsys, 0, pgl, ms,&
                        ng, lg, ir, q)
        endif
!
        call tbajli(comdes, 5, nompar, [0], [0.d0],&
                    [cbid], noms, 0)
        do 11 j = 1, 5
            zk24(imk-1+(iocc-1)*5+j)=noms(j)
11      continue
        ir=0
        nvi=nvi+4*nbsys
        nbsyst=nbsyst+nbsys
!
 9  end do
!
    zr(ipr)=nbtbsg
    zr(ipr+1)=decal+1
!     INDICATEUR PLASTIQUE
    nvi=nvi+1
!     CONTRAINTE DE CLIVAGE MAX
    nvi=nvi+1
!     ROTATION DE RESEAU
    call getvtx(' ', 'ROTA_RESEAU', scal=rota, nbret=nbrota)
    irota=0
    if (nbrota .ne. 0) then
        if (rota .ne. 'NON') then
            if (rota .eq. 'POST') irota=1
            if (rota .eq. 'CALC') irota=2
        endif
        if (irota .gt. 0) nvi = nvi+16
    endif
!     RHO_IRR
    if (irra .gt. 0) then
        nvi=nvi+12*nboccm
    endif
!     RHO_IRR
    if (irr2 .gt. 0) then
        nvi=nvi+24*nboccm
    endif
!
    noms(1)='MONOCRISTAL'
    noms(2)=ecoule
    nums(1)=nboccm
    nums(2)=nvi
    call utmess('I', 'COMPOR2_23', nk=2, valk=noms, ni=2,&
                vali=nums)
!
    nomvar(1)='EPSPXX'
    nomvar(2)='EPSPYY'
    nomvar(3)='EPSPZZ'
    nomvar(4)='EPSPXY'
    nomvar(5)='EPSPXZ'
    nomvar(6)='EPSPYZ'
    do 554 i = 1, nbsyst
        call codent(i, 'G', chaine)
        nomvar(6+3*i-2)='ALPHA'//chaine
        nomvar(6+3*i-1)='GAMMA'//chaine
        nomvar(6+3*i )='P'//chaine
554  end do
    if (irra .gt. 0) then
        do 557 i = 1, 12*nboccm
            call codent(i, 'G', chaine)
            nomvar(6+3*nbsyst+i)='RHO_IRRA_'//chaine
557      continue
    endif
!
    if (irr2 .gt. 0) then
        do 559 i = 1, 12*nboccm
            call codent(i, 'G', chaine)
            nomvar(6+3*nbsyst+i)='RHO_LOOPS_'//chaine
            call codent(i, 'G', chaine)
            nomvar(6+4*nbsyst+i)='PHI_VOIDS_'//chaine
559      continue
    endif
!
    indvar=6+3*nbsyst
    if (irra .gt. 0) indvar=indvar+12*nboccm
!
    if (irr2 .gt. 0) indvar=indvar+24*nboccm
!
    if (irota .gt. 0) then
        do 556 i = 1, 16
            call codent(i, 'G', chaine)
            nomvar(indvar+i)='ROTA_'//chaine
556      continue
        indvar=indvar+16
    endif
!
    do 558 i = 1, nbsyst
        call codent(i, 'G', chaine)
        nomvar(indvar+i)='TAU_'//chaine
558  end do
!
    nomvar(nvi-2)='SIGM_CLIV'
    nomvar(nvi-1)='EPSPEQ'
    nomvar(nvi)='NBITER'
!
    do 555 i = 1, nvi
        call utmess('I', 'COMPOR2_24', sk=nomvar(i), si=i)
555  end do
!
!
    zk24(imk+5*nboccm)=elasti
    call getvid(' ', 'MATR_INTER', scal=tbinte, nbret=itab)
    if (itab .ne. 0) then
        listr = '&&LCMMAT.TABL_INTER'
        call tbexlr(tbinte, listr, 'V')
        call jeveuo(listr//'.VALE', 'L', iadlr)
!        VERIF QUE LA MATRICE EST CARREE
        if (zr(iadlr+1) .ne. zr(iadlr+2)) then
            call utmess('F', 'COMPOR2_15', nr=2, valr=zr(iadlr+1))
        endif
!        VERIF QUE LE NB DE SYST EST OK
        if (zr(iadlr+1) .ne. nbsyst) then
            call utmess('F', 'COMPOR2_17', si=nbsyst)
        endif
        call dcopy(nbsyst*nbsyst, zr(iadlr+3), 1, zr(ipr+decal), 1)
!        VERIF QUE LA MATRICE EST SYMETRIQUE
        do 5 i = 1, nbsyst
            do 5 j = 1, nbsyst
                if (zr(ipr-1+decal+nbsyst*(i-1)+j) .ne. zr(ipr-1+decal+ nbsyst*(j-1)+i)) then
                    call utmess('F', 'COMPOR2_18')
                endif
 5          continue
        call jedetc('V', listr, 1)
!
        if (niv .eq. 2) then
            write(ifm,*) ' MATRICE INTERACTION UTILISATEUR'
            do 6 i = 1, nbsyst
                write(ifm,'(I2,12(1X,E11.4))') i,(zr(ipr-1+decal+&
                nbsyst*(i-1)+j),j=1,nbsyst)
 6          continue
        endif
    else
        if (nboccm .gt. 1) then
            call utmess('F', 'COMPOR2_20', si=nbsyst)
        endif
    endif
    tabdes(1)=1
    tabdes(2)=1
    tabdes(3)=nvi
    tabdes(4)=itab
    tabdes(5)=nboccm
    tabdes(6)=irota
    tabdes(7)=nvi
    tabdes(8)=nbsyst
!
!     organisation de CPRI :
!     1 : TYPE =1 pour MONOCRISTAL
!     2 : NBPHAS=1 pour MONOCRISTAL
!     3 : NVI
!     4 : NOMBRE DE MONOCRISTAUX différents  =1
!     5 : NBFAMILLES DE SYS GLIS
!     6 : 1 si ROTA=POST, 2 si CALC, 0 sinon
!     7 : NVI
!     8 : NOMBRE DE SYSTEMES DE GLISSEMENT TOTAL
!
    call wkvect(compor//'.CPRI', 'G V I', 13, imi)
    do 999 i = 1, 13
        zi(imi+i-1)=tabdes(i)
999  end do
    call detrsd('TABLE', comdes)
!
! FIN ------------------------------------------------------------------
!
    call jedema()
end subroutine
