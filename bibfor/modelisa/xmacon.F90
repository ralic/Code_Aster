subroutine xmacon(char, noma, nomo)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/celces.h"
#include "asterfort/cesexi.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/ismali.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/mminfi.h"
#include "asterfort/teattr.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/xmelin.h"
!
    character(len=8) :: char, noma, nomo
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE XFEM - LECTURE DONNEES)
! CREATION DES SDS SPECIFIQUES FORMULATION XFEM
!
!
! ----------------------------------------------------------------------
!
! ----------------------------------------------------------------------
! ROUTINE SPECIFIQUE A L'APPROCHE <<GRANDS GLISSEMENTS AVEC XFEM>>,
! TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
! ----------------------------------------------------------------------
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  NOMA   : NOM DU MAILLAGE
! IN  NOMO   : NOM DU MODELE
!
!
!
!
    character(len=24) :: defico
    character(len=24) :: ndimco, xfimai, maescx
    integer :: jdim, jfimai, jmaesx
    integer :: zmesx
    integer :: ndim, jtypma, itypma
    integer :: nzoco
    integer :: ntmae, nbma, ntmano, nface, ninter
    integer :: jcesd2,  jcesl2
    integer :: jconx1, jconx2
    integer :: jcesd, jcesl,  jmail, ityele
    integer :: izone, ima, ntpc, iad, posmae
    integer :: iad1, iad2, statut, ibid, nfiss, ifiss
    character(len=8) :: nomfis, nomzon, typma, elrefe
    character(len=19) :: chs, faclon, chs2, typmai, maille
    character(len=16) :: typele, enr
    logical :: lmalin
    integer :: typint, nnint
    integer, pointer :: cesv2(:) => null()
    character(len=8), pointer :: cesv(:) => null()
    integer, pointer :: xfem_cont(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    defico = char(1:8)//'.CONTACT'
    ntpc = 0
    nzoco = cfdisi(defico,'NZOCO' )
    chs = '&&XMACON.CHS'
    chs2 = '&&XMACON.CHS2'
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    xfimai = defico(1:16)//'.XFIMAI'
    ndimco = defico(1:16)//'.NDIMCO'
    call jeveuo(ndimco, 'E', jdim)
    call jeveuo(xfimai, 'L', jfimai)
    call jeveuo(nomo//'.XFEM_CONT', 'L', vi=xfem_cont)
!
! --- ON RECUPERE LE NOMBRE TOTAL DE MAILLES DU MAILLAGE ET SA DIMENSION
!
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbma)
    call dismoi('DIM_GEOM', noma, 'MAILLAGE', repi=ndim)
!
! --- ON RECUPERE LA CONNECTIVITE DU MAILLAGE
!
    call jeveuo(noma//'.CONNEX', 'L', jconx1)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
    typmai = noma//'.TYPMAIL'
    maille = nomo//'.MAILLE'
    call jeveuo(typmai, 'L', jtypma)
    call jeveuo(maille, 'L', jmail)
!
! --- ON RECUPERE LE MAPPING MAILLE XFEM - NOMS FISSURES
!
    call celces(nomo//'.XMAFIS', 'V', chs)
    call jeveuo(chs//'.CESD', 'L', jcesd)
    call jeveuo(chs//'.CESV', 'L', vk8=cesv)
    call jeveuo(chs//'.CESL', 'L', jcesl)
!
! --- ON TRANSFORME LE CHAMP TOPOFAC.LO EN CHAMP SIMPLE
!
    faclon = nomo//'.TOPOFAC.LO'
    call celces(faclon, 'V', chs2)
    call jeveuo(chs2//'.CESD', 'L', jcesd2)
    call jeveuo(chs2//'.CESV', 'L', vi=cesv2)
    call jeveuo(chs2//'.CESL', 'L', jcesl2)
!
! --- NOMBRE TOTAL DES MAILLES ESCLAVES DE CONTACT.
!
    ntmae = 0
    do ima = 1, nbma
        call cesexi('C', jcesd, jcesl, ima, 1,&
                    1, 1, iad)
        if (iad .gt. 0) then
!          ITYELE=ZI(JMAIL-1+IMA)
!          CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',ITYELE),TYPELE)
!          CALL TEATTR('S','XFEM',ENR,IBID)
!          IF (ENR(3:3).EQ.'C'.OR.ENR(4:4).EQ.'C') THEN
            nfiss = zi(jcesd-1+5+4*(ima-1)+2)
            do ifiss = 1, nfiss
                call cesexi('C', jcesd2, jcesl2, ima, 1,&
                            ifiss, 1, iad2)
                ninter = cesv2(iad2)
                if (ninter .gt. 0) ntmae=ntmae+1
            end do
!          ENDIF
        endif
    end do
!
!
! --- CREATION DU TABLEAU DES MAILLES ESCLAVES
!
    maescx = defico(1:16)//'.MAESCX'
    zmesx = cfmmvd('ZMESX')
    call wkvect(maescx, 'G V I', zmesx*ntmae, jmaesx)
!
! --- REMPLISSAGE DU TABLEAU DES MAILLES ESCLAVES
!
    posmae = 0
!
    do izone = 1, nzoco
!
! --- ON RECUPERE LE NOMBRE DE POINTS D'INTEGRATION PAR FACETTE
        if (ndim .eq. 2) then
            if (xfem_cont(1) .le. 2) elrefe='SE2'
            if (xfem_cont(1) .eq. 3) elrefe='SE3'
        else if (ndim.eq.3) then
            elrefe='TR3'
        endif
!
        typint = mminfi(defico,'INTEGRATION',izone )
        call xmelin(elrefe, typint, nnint)
!
        nomzon = zk8(jfimai-1+izone)
        do ima = 1, nbma
! --- ON VERIFIE QUE C'EST UNE MAILLE X-FEM AVEC CONTACT
            call cesexi('C', jcesd, jcesl, ima, 1,&
                        1, 1, iad)
            if (iad .eq. 0) goto 210
! --- RECUPERATION DU NUMÉRO DE FISSURE LOCAL
            nfiss = zi(jcesd-1+5+4*(ima-1)+2)
            do ifiss = 1, nfiss
                call cesexi('C', jcesd, jcesl, ima, 1,&
                            ifiss, 1, iad1)
                nomfis = cesv(iad1)
                if (nomzon .eq. nomfis) goto 230
            end do
! --- ON SORT SI LA MAILLE NE CONTIENT PAS LA FISSURE EN COURS
            goto 210
230         continue
!
! --- ON SORT SI PAS DE POINTS D'INTERSECTIONS
            call cesexi('C', jcesd2, jcesl2, ima, 1,&
                        ifiss, 1, iad2)
            ninter = cesv2(iad2)
            if (ninter .eq. 0) goto 210
! --- ON RECUPERE LE NOMBRE DE FACETTES DE CONTACT
            call cesexi('C', jcesd2, jcesl2, ima, 1,&
                        ifiss, 2, iad2)
            nface = cesv2(iad2)
!
            ityele=zi(jmail-1+ima)
            call jenuno(jexnum('&CATA.TE.NOMTE', ityele), typele)
            call teattr('S', 'XFEM', enr, ibid, typel=typele)
! --- ON SORT SI CE N'EST PAS UNE MAILLE DE CONTACT
!          IF (ENR(3:3).NE.'C'.AND.ENR(4:4).NE.'C') GOTO 210
! --- CALCUL DU STATUT DE LA MAILLE, UTILE POUR LA PROJECTION :
! ---  1 SI HEAVISIDE
! ---  2 SI CRACK-TIP
! ---  3 SI HEAVISIDE CRACK-TIP
!
            if (enr(2:2) .eq. 'H') statut = 1
            if (enr(2:2) .eq. 'T') statut = 2
            if (enr(3:3) .eq. 'T') statut = 3
!
! --- ON VERIFIE QUE LA MAILLE EST LINEAIRE SI 3D OU CRACK TIP
            itypma = zi(jtypma-1+ima)
            call jenuno(jexnum('&CATA.TM.NOMTM', itypma), typma)
            lmalin = ismali(typma)
            if (.not.lmalin) then
                if (statut .gt. 1) then
                    call utmess('F', 'XFEM_38')
                endif
            endif
!
            posmae = posmae+1
            zi(jmaesx+zmesx*(posmae-1)+1-1) = ima
            zi(jmaesx+zmesx*(posmae-1)+2-1) = izone
            zi(jmaesx+zmesx*(posmae-1)+3-1) = nnint
            zi(jmaesx+zmesx*(posmae-1)+4-1) = statut
            zi(jmaesx+zmesx*(posmae-1)+5-1) = ifiss
            ntpc = ntpc + nnint*nface
            if (nface .eq. 0) then
                ntpc = ntpc + 1
                zi(jmaesx+zmesx*(posmae-1)+3-1) = 1
                zi(jmaesx+zmesx*(posmae-1)+4-1) = -1*statut
            endif
210         continue
        end do
    end do
!
! --- NOMBRE DE MAILLES ESCLAVES
!
    ntmae = posmae
    call jeecra(maescx, 'LONUTI', zmesx*ntmae)
    zi(jdim+9 -1) = ntmae
    zi(jdim+13-1) = ntmae
!
! --- NOMBRE DE POINTS ESCLAVES
!
    zi(jdim+16-1) = ntpc
    zi(jdim+17-1) = ntpc
!
! --- NOMBRE TOTAL DE NOEUD AUX ELEMENTS (ELNO)
!
    ntmano = 0
    zi(jdim+18-1) = ntmano
!
! --- MENAGE
!
    call detrsd('CHAM_ELEM_S', chs)
    call detrsd('CHAM_ELEM_S', chs2)
!
    call jedema()
end subroutine
