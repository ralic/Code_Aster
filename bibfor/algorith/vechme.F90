subroutine vechme(stop, modelz, chargz, infchz, inst,&
                  carele, mate, vrcplu, ligrez, vecelz)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: jacques.pellet at edf.fr
!
    implicit      none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/copisd.h"
#include "asterfort/corich.h"
#include "asterfort/detrsd.h"
#include "asterfort/exisd.h"
#include "asterfort/exixfe.h"
#include "asterfort/gcnco2.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecact.h"
#include "asterfort/mecara.h"
#include "asterfort/megeom.h"
#include "asterfort/memare.h"
#include "asterfort/nmdepr.h"
#include "asterfort/reajre.h"
    character(len=*) :: modelz, chargz, infchz, carele, mate
    character(len=*) :: vrcplu, vecelz, ligrez
    character(len=1) :: stop
    real(kind=8) :: inst(3)
!
! ----------------------------------------------------------------------
!
!  CALCUL DES VECTEURS ELEMENTAIRES DES CHARGEMENTS MECANIQUES
!  DE NEUMANN NON SUIVEURS ET NON PILOTABLES (CONSTANTS).
!
! ----------------------------------------------------------------------
!
!  PRODUIT UN VECT_ELEM DEVANT ETRE ASSEMBLE PAR LA ROUTINE ASASVE
!
! IN  STOP   : COMPORTEMENT DE CALCUL
! IN  MODELE : NOM DU MODELE
! IN  CHARGE : LISTE DES CHARGES
! IN  INFCHA : INFORMATIONS SUR LES CHARGEMENTS
! IN  INST   : TABLEAU DONNANT T+, DELTAT ET THETA (POUR LE THM)
! IN  CARELE : CARACTERISTIQUES DES POUTRES ET COQUES
! IN  MATE   : MATERIAU CODE
! IN  TEMPLU : CHAMP DE TEMPERATURE A L'INSTANT T+
! IN  LIGREZ : (SOUS-)LIGREL DE MODELE POUR CALCUL REDUIT
!                  SI ' ', ON PREND LE LIGREL DU MODELE
! OUT VECELE : VECT_ELEM RESULTAT.
!
! ATTENTION :
! -----------
!   LE VECT_ELEM (VECELZ) RESULTAT A 1 PARTICULARITE :
!   1) CERTAINS RESUELEM NE SONT PAS DES RESUELEM MAIS DES
!      CHAM_NO (.VEASS)
    integer :: nchinx
    parameter (nchinx=42)
    integer :: nbchmx
    parameter (nbchmx=17)
    integer :: jlchin, isigi
    integer :: ier, jchar, jinf
    integer :: ibid, iret, nchar, k, icha, ii, iexis
    integer :: numchm, nchin
    character(len=5) :: suffix
    character(len=6) :: nomlig(nbchmx), nompaf(nbchmx), nompar(nbchmx)
    character(len=6) :: nomopf(nbchmx), nomopr(nbchmx)
    character(len=7) :: nomcmp(3)
    character(len=8) :: nomcha, k8bid
    character(len=8) :: lpain(nchinx), lpaout, newnom, modele
    character(len=16) :: option
    character(len=24) :: chgeom, chcara(18), chtime, ligrel
    character(len=24) :: ligrmo, ligrch
    character(len=19) :: lchout, resufv(3), vecele
    character(len=24) :: lchin(nchinx)
    character(len=24) :: charge, infcha
    logical :: exicar, bidon, lxfem
    complex(kind=8) :: cbid
!
    data nomlig/'.FORNO','.F3D3D','.F2D3D','.F1D3D','.F2D2D','.F1D2D',&
     &     '.F1D1D','.PESAN','.ROTAT','.PRESS','.FELEC','.FCO3D',&
     &     '.FCO2D','.EPSIN','.FLUX','.VEASS','.SIINT'/
    data nomopf/'FORC_F','FF3D3D','FF2D3D','FF1D3D','FF2D2D','FF1D2D',&
     &     'FF1D1D','PESA_R','ROTA_R','PRES_F','FRELEC','FFCO3D',&
     &     'FFCO2D','EPSI_F','FLUX_F','      ',' '/
    data nompaf/'FORNOF','FF3D3D','FF2D3D','FF1D3D','FF2D2D','FF1D2D',&
     &     'FF1D1D','PESANR','ROTATR','PRESSF','FRELEC','FFCO3D',&
     &     'FFCO2D','EPSINF','FLUXF','      ',' '/
    data nomopr/'FORC_R','FR3D3D','FR2D3D','FR1D3D','FR2D2D','FR1D2D',&
     &     'FR1D1D','PESA_R','ROTA_R','PRES_R','FRELEC','FRCO3D',&
     &     'FRCO2D','EPSI_R','FLUX_R','      ',' '/
    data nompar/'FORNOR','FR3D3D','FR2D3D','FR1D3D','FR2D2D','FR1D2D',&
     &     'FR1D1D','PESANR','ROTATR','PRESSR','FRELEC','FRCO3D',&
     &     'FRCO2D','EPSINR','FLUXR','      ',' '/
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    newnom = '.0000000'
    modele = modelz
    charge = chargz
    infcha = infchz
    ligrmo = ligrez
    do 10 ii = 1, nchinx
        lchin(ii) = ' '
        lpain(ii) = ' '
10  end do
    call exixfe(modele, ier)
    lxfem = ier.ne.0
    if (ligrmo .eq. ' ') ligrmo = modele(1:8)//'.MODELE'
    lpaout = 'PVECTUR'
    lchout = '&&VECHME.???????'
!
! --- CALCUL DU NOM DU RESULTAT :
!
    vecele = vecelz
    if (vecele .eq. ' ') vecele = '&&VEMCHA'
!
! --- DETECTION DE LA PRESENCE DE CHARGES
!
    bidon = .true.
    call jeexin(charge, iret)
    if (iret .ne. 0) then
        call jelira(charge, 'LONMAX', nchar, k8bid)
        if (nchar .ne. 0) then
            bidon = .false.
            call jeveuo(charge, 'L', jchar)
            call jeveuo(infcha, 'L', jinf)
        endif
    endif
!
! --- ALLOCATION DU VECT_ELEM RESULTAT :
!
    call detrsd('VECT_ELEM', vecele)
    call memare('V', vecele, modele, mate, carele,&
                'CHAR_MECA')
    call reajre(vecele, ' ', 'V')
    if (bidon) goto 99
!
! --- CARTES GEOMETRIE ET CARA_ELEM
!
    call megeom(modele, chgeom)
    call mecara(carele, exicar, chcara)
!
! --- CARTE INSTANTS
!
    chtime = '&&VECHME.CH_INST_R'
    nomcmp(1) = 'INST   '
    nomcmp(2) = 'DELTAT '
    nomcmp(3) = 'THETA  '
    call mecact('V', chtime, 'LIGREL', ligrmo, 'INST_R  ',&
                3, nomcmp, ibid, inst, cbid,&
                k8bid)
!
! --- CHAMPS IN
!
    lpain(2) = 'PGEOMER'
    lchin(2) = chgeom
    lpain(3) = 'PTEMPSR'
    lchin(3) = chtime
    lpain(4) = 'PMATERC'
    lchin(4) = mate
    lpain(5) = 'PCACOQU'
    lchin(5) = chcara(7)
    lpain(6) = 'PCAGNPO'
    lchin(6) = chcara(6)
    lpain(7) = 'PCADISM'
    lchin(7) = chcara(3)
    lpain(8) = 'PCAORIE'
    lchin(8) = chcara(1)
    lpain(9) = 'PCACABL'
    lchin(9) = chcara(10)
    lpain(10) = 'PCAARPO'
    lchin(10) = chcara(9)
    lpain(11) = 'PCAGNBA'
    lchin(11) = chcara(11)
    lchin(12) = vrcplu
    lpain(12) = 'PVARCPR '
    lpain(13) = 'PCAMASS'
    lchin(13) = chcara(12)
    lpain(14) = 'PCAGEPO'
    lchin(14) = chcara(5)
    lpain(15) = 'PNBSP_I'
    lchin(15) = chcara(16)
    lpain(16) = 'PFIBRES'
    lchin(16) = chcara(17)
    lpain(17) = 'PCINFDI'
    lchin(17) = chcara(15)
    lpain(18) = 'PCOMPOR'
    lchin(18) = mate(1:8)//'.COMPOR'
    nchin = 18
    if (lxfem) then
        lpain(nchin + 1) = 'PPINTTO'
        lchin(nchin + 1) = modele(1:8)//'.TOPOSE.PIN'
        lpain(nchin + 2) = 'PCNSETO'
        lchin(nchin + 2) = modele(1:8)//'.TOPOSE.CNS'
        lpain(nchin + 3) = 'PHEAVTO'
        lchin(nchin + 3) = modele(1:8)//'.TOPOSE.HEA'
        lpain(nchin + 4) = 'PLONCHA'
        lchin(nchin + 4) = modele(1:8)//'.TOPOSE.LON'
        lpain(nchin + 5) = 'PLSN'
        lchin(nchin + 5) = modele(1:8)//'.LNNO'
        lpain(nchin + 6) = 'PLST'
        lchin(nchin + 6) = modele(1:8)//'.LTNO'
        lpain(nchin + 7) = 'PSTANO'
        lchin(nchin + 7) = modele(1:8)//'.STNO'
        lpain(nchin + 8) = 'PPMILTO'
        lchin(nchin + 8) = modele(1:8)//'.TOPOSE.PMI'
        lpain(nchin + 9) = 'PFISNO'
        lchin(nchin + 9) = modele(1:8)//'.FISSNO'
        nchin = nchin + 9
    endif
!
! --- CALCUL
!
    do 70 icha = 1, nchar
        numchm = zi(jinf+nchar+icha)
        if (numchm .gt. 0) then
            nomcha = zk24(jchar+icha-1) (1:8)
            ligrch = nomcha//'.CHME.LIGRE'
!
! ------- LE LIGREL UTILISE DANS CALCUL EST LE LIGREL DU MODELE
! ------- SAUF POUR LES FORCES NODALES
!
            do 40 k = 1, nbchmx
                if (nomlig(k) .eq. '.FORNO') then
                    ligrel = ligrch
                else
                    ligrel = ligrmo
                endif
                if (nomlig(k) .eq. '.VEASS') then
                    suffix = '     '
                else
                    suffix = '.DESC'
                endif
                lchin(1) = nomcha//'.CHME'//nomlig(k)//suffix
                call jeexin(lchin(1), iret)
                if (iret .ne. 0) then
                    if (numchm .eq. 1) then
                        option = 'CHAR_MECA_'//nomopr(k)
                        lpain(1) = 'P'//nompar(k)
                    else if (numchm.eq.2) then
                        option = 'CHAR_MECA_'//nomopf(k)
                        lpain(1) = 'P'//nompaf(k)
                    else if (numchm.eq.3) then
                        option = 'CHAR_MECA_'//nomopf(k)
                        lpain(1) = 'P'//nompaf(k)
                    else if (numchm.eq.55) then
                        option = 'FORC_NODA'
                        call jeveuo(ligrch(1:13)//'.SIINT.VALE', 'L', isigi)
                        lpain(1) = 'PCONTMR'
                        lchin(1) = zk8(isigi)
                        nchin = nchin + 1
                        lpain(nchin) = 'PDEPLMR'
                        lchin(nchin) = ' '
                    else if (numchm.ge.4) then
                        goto 40
                    endif
!
                    nchin = 28
!
! ----------- POUR LES ELEMENTS DE BORD XFEM
!
                    if (lxfem) then
                        if (option .eq. 'CHAR_MECA_PRES_R' .or. option .eq.&
                            'CHAR_MECA_PRES_F') then
                            lpain(nchin + 1) = 'PPINTER'
                            lchin(nchin + 1) = modele(1:8)// '.TOPOFAC.OE'
                            lpain(nchin + 2) = 'PAINTER'
                            lchin(nchin + 2) = modele(1:8)// '.TOPOFAC.AI'
                            lpain(nchin + 3) = 'PCFACE'
                            lchin(nchin + 3) = modele(1:8)// '.TOPOFAC.CF'
                            lpain(nchin + 4) = 'PLONGCO'
                            lchin(nchin + 4) = modele(1:8)// '.TOPOFAC.LO'
                            lpain(nchin + 5) = 'PBASECO'
                            lchin(nchin + 5) = modele(1:8)// '.TOPOFAC.BA'
                            nchin = nchin + 5
                        endif
                    endif
!
! ----------- GENERATION NOM DU RESU_ELEM EN SORTIE
!
                    call gcnco2(newnom)
                    lchout(10:16) = newnom(2:8)
                    call corich('E', lchout, icha, ibid)
!
! ----------- SI .VEASS, IL N'Y A PAS DE CALCUL A LANCER
!
                    if (nomlig(k) .eq. '.VEASS') then
                        call jeveuo(lchin(1), 'L', jlchin)
                        call copisd('CHAMP_GD', 'V', zk8(jlchin), lchout)
                    else
                        ASSERT(nchin.le.nchinx)
                        call calcul(stop, option, ligrel, nchin, lchin,&
                                    lpain, 1, lchout, lpaout, 'V',&
                                    'OUI')
                    endif
!
! ----------- RECOPIE DU CHAMP (S'IL EXISTE) DANS LE VECT_ELEM
!
                    call exisd('CHAMP_GD', lchout, iexis)
                    ASSERT((iexis.gt.0).or.(stop.eq.'C'))
                    call reajre(vecele, lchout, 'V')
!
                endif
40          continue
        endif
!
! ----- TRAITEMENT DE AFFE_CHAR_MECA/EVOL_CHAR
!
        do 50 ii = 1, 3
            resufv(ii) = lchout
            call gcnco2(newnom)
            resufv(ii) (10:16) = newnom(2:8)
50      continue
        call nmdepr(modelz, ligrmo, carele, chargz, icha,&
                    inst(1), resufv)
        do 60 ii = 1, 3
            call reajre(vecele, resufv(ii), 'V')
60      continue
70  end do
99  continue
!
    vecelz = vecele//'.RELR'
    call jedema()
end subroutine
