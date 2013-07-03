subroutine nmdocc(compor, modele, nbmo1, moclef, nomcmp,&
                  ncmpma, meca, nomcmd)
! person_in_charge: jean-michel.proix at edf.fr
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     SAISIE ET STOCKAGE DES PARAMETRES LOCAUX DE COMPORTEMENT
!
! OUT COMPOR : CARTE DECRIVANT LES PARAMETRES K16 DU COMPORTEMENT
! IN MODELE  : LE MODELE
! IN NBMO1   : NOMBRE DE MOTS-CLES (1 OU 2) COMP_INCR / COMP_ELAS
! IN MOCLEF  : LISTE DES MOTS-CLES (COMP_INCR / COMP_ELAS)
! IN NCMPMA  : NOMBRE DE CMP DE LA GRANDEUR COMPOR
! IN NOMCMP  : NOMS DES CMP DE LA GRANDEUR COMPOR
! IN MECA    : COMMANDE MECANIQUE OU PAS
! IN NOMCMD  : NOMS DE LA COMMANDE
! ----------------------------------------------------------------------
    implicit none
#include "jeveux.h"
#include "asterc/getexm.h"
#include "asterc/getfac.h"
#include "asterc/getvid.h"
#include "asterc/getvis.h"
#include "asterc/getvtx.h"
#include "asterc/lccree.h"
#include "asterc/lcinfo.h"
#include "asterc/lctest.h"
#include "asterc/zaswri.h"
#include "asterfort/alcart.h"
#include "asterfort/crcmel.h"
#include "asterfort/dismoi.h"
#include "asterfort/imvari.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmdocp.h"
#include "asterfort/nmdogd.h"
#include "asterfort/nmdoki.h"
#include "asterfort/nmdovd.h"
#include "asterfort/nmdovm.h"
#include "asterfort/nmdpmf.h"
#include "asterfort/nocart.h"
#include "asterfort/reliem.h"
#include "asterfort/u2mesg.h"
    integer :: icmp, k, jma, nbma, iret, i, ibid, n1, jvalv, ncmpma, jncmp
    integer :: nbmo1, nbocc, dimaki, dimanv, nbkit, numlc, nbvari, icpri
    integer :: nbvarz, nunit, ii, inv, iarg, indimp
!    DIMAKI = DIMENSION MAX DE LA LISTE DES RELATIONS KIT
    parameter (dimaki=9)
!    DIMANV = DIMENSION MAX DE LA LISTE DU NOMBRE DE VAR INT EN THM
    parameter (dimanv=4)
    integer :: nbnvi(dimanv), ncomel, nvmeta, nt
    character(len=8) :: noma, k8b, typmcl(2), nomcmp(*), sdcomp, tavari
    character(len=16) :: tymatg, moclef(2), comp, txcp, defo, mocles(2)
    character(len=16) :: texte(2), comcod, lcomel(10), nomkit(dimaki)
    character(len=16) :: nomsub, nomcmd, comco2, crirup
    character(len=19) :: ces2, compor
    character(len=24) :: mesmai, modele
    character(len=128) :: nomlib
    integer :: exist
    logical :: exipmf, meca, iszmat
!
    save indimp
    data indimp /1/
!
! ----------------------------------------------------------------------
    call jemarq()
!
    exipmf = .false.
    iszmat = .false.
    compor = '&&NMDOCC.COMPOR'
!
    call dismoi('F', 'NOM_MAILLA', modele(1:8), 'MODELE', i,&
                noma, iret)
!
! ======================================================================
    if (meca) then
!        ON INITIALISE LA CARTE COMPOR AVEC 'ELAS' SUR TOUT LE MAILLAGE
        ces2='&&NMDOCC.CES2'
        call crcmel(nbmo1, moclef, compor, ces2, modele,&
                    ncmpma, nomcmp, nt)
!
!        SI COMP_ELAS ET COMP_INCR SONT ABSENTS (POUR CALC_G)
        if (nt .eq. 0) then
            if (nomcmd(1:6) .eq. 'CALC_G') then
                goto 170
            else if (nomcmd(1:9).eq.'LIRE_RESU') then
!                       1234567890123456789
                compor = '                   '
                goto 170
            endif
        endif
    else
        call alcart('V', compor, noma, 'COMPOR')
    endif
! ======================================================================
!
    mocles(1) = 'GROUP_MA'
    mocles(2) = 'MAILLE'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'MAILLE'
    mesmai = '&&NMDOCC'//'.MES_MAILLES'
    txcp='ANALYTIQUE'
!
! ======================================================================
!                       REMPLISSAGE DE LA CARTE COMPOR :
! --- ON STOCKE LE NOMBRE DE VARIABLES INTERNES PAR COMPORTEMENT
! ======================================================================
!
    call jeveuo(compor//'.NCMP', 'E', jncmp)
    call jeveuo(compor//'.VALV', 'E', jvalv)
    do 90 icmp = 1, ncmpma
        zk8(jncmp+icmp-1) = nomcmp(icmp)
90  end do
!
!     MOTS CLES FACTEUR
    do 160 i = 1, nbmo1
        call getfac(moclef(i), nbocc)
!
!       NOMBRE D'OCCURRENCES
        do 150 k = 1, nbocc
!
            call getvtx(moclef(i), 'RELATION', k, iarg, 1,&
                        comp, n1)
            ncomel=1
            lcomel(ncomel)=comp
!
            call reliem(modele, noma, 'NU_MAILLE', moclef(i), k,&
                        2, mocles, typmcl, mesmai, nbma)
!
!         VERIFICATIONS DE LA COMPATIBILITE MODELE-COMPORTEMENT
!         AFFECTATION AUTOMATIQUE DE DEBORST LE CAS ECHEANT
            if (meca) then
                call lccree(ncomel, lcomel, comco2)
                call nmdovm(modele, mesmai, nbma, ces2, comco2,&
                            comp, txcp)
            endif
!
!         SAISIE ET VERIFICATION DU TYPE DE DEFORMATION UTILISEE
            call nmdogd(moclef(i), comp, k, ncomel, lcomel,&
                        defo)
!
!         VERIFICATIONS DE LA COMPATIBILITE MODELE-DEFORMATION
            if (meca) then
                call lccree(1, lcomel(2), comco2)
                call nmdovd(modele, mesmai, nbma, ces2, comco2,&
                            lcomel(2))
            endif
!
!         POUR COMPORTEMENTS KIT_
            call nmdoki(moclef(i), modele, comp, k, dimaki,&
                        nbkit, nomkit, nbnvi, ncomel, lcomel,&
                        numlc, nvmeta)
!
!         PRISE EN COMPTE DE DEBORST
            call nmdocp(ncomel, lcomel, txcp)
!
!         APPEL A LCINFO POUR RECUPERER LE NOMBRE DE VARIABLES INTERNES
            call lccree(ncomel, lcomel, comcod)
            call lcinfo(comcod, numlc, nbvari)
!
            if (comp .eq. 'ROUSSELIER') then
                nbvari=nbvari-6
            endif
!
!         NOMS DES VARIABLES INTERNES
            if (indimp .eq. 1) then
                call imvari(moclef(i), k, ncomel, lcomel, comcod,&
                            nbvari, tavari)
            endif
!
!         CAS PARTICULIER DE META A INTEGRER DANS CATA_COMPORTEMENT.PY
            if (comp(1:4) .eq. 'META') then
                if (defo .eq. 'SIMO_MIEHE') nvmeta=nvmeta+1
                if (defo .eq. 'GDEF_LOG') nvmeta=nvmeta+6
                nbvari=nvmeta
            endif
!
!         VERIF QUE DEFO EST POSSIBLE POUR COMP
            call lctest(comcod, 'DEFORMATION', defo, iret)
            if (iret .eq. 0) then
                texte(1)=defo
                texte(2)=comp
                call u2mesg('F', 'COMPOR1_44', 2, texte, 0,&
                            0, 0, 0.d0)
            endif
!
! ======================================================================
!         CAS PARTICULIERS
            tymatg=' '
            exist = getexm(moclef(i),'TYPE_MATR_TANG')
            if (exist .eq. 1) then
                call getvtx(moclef(i), 'TYPE_MATR_TANG', k, iarg, 1,&
                            tymatg, n1)
                if (n1 .gt. 0) then
                    if (tymatg .eq. 'TANGENTE_SECANTE') nbvari=nbvari+1
                endif
            endif
!         CAS PARTICULIER DE MONOCRISTAL
            if (comp(1:8) .eq. 'MONOCRIS') then
                call getvid(moclef(i), 'COMPOR', k, iarg, 1,&
                            sdcomp, n1)
                call jeveuo(sdcomp//'.CPRI', 'L', icpri)
                nbvari=zi(icpri-1+3)
                zk16(jvalv-1+7) = sdcomp
                if (txcp .eq. 'DEBORST') nbvari=nbvari+4
                if ((defo.eq.'SIMO_MIEHE') .and. (comp.eq.'MONOCRISTAL')) then
                    nbvari=nbvari+9+9
                endif
            else if (comp(1:8).eq.'POLYCRIS') then
                call getvid(moclef(i), 'COMPOR', k, iarg, 1,&
                            sdcomp, n1)
                call jeveuo(sdcomp//'.CPRI', 'L', icpri)
                nbvari=zi(icpri-1+3)
                zk16(jvalv-1+7) = sdcomp
                if (txcp .eq. 'DEBORST') nbvari=nbvari+4
            endif
!
!         STOCKAGE DE VI SI POST_ITER='CRIT_RUPT'
            if (nomcmd(1:6) .ne. 'CALC_G') then
                if (moclef(i) .eq. 'COMP_INCR') then
                    call getvtx(moclef(i), 'POST_ITER', k, iarg, 1,&
                                crirup, iret)
                    if (iret .eq. 1) then
                        nbvari=nbvari+6
                    endif
                endif
            endif
!
            if (comp(1:8) .eq. 'MULTIFIB') exipmf=.true.
            if (comp(1:4) .eq. 'ZMAT') then
                iszmat = .true.
                call getvis(moclef, 'NB_VARI', k, iarg, 1,&
                            nbvarz, n1)
                nbvari=nbvarz+nbvari
                call getvis(moclef, 'UNITE', k, iarg, 1,&
                            nunit, n1)
                write (zk16(jvalv-1+7),'(I16)') nunit
            endif
!         POUR COMPORTEMENT KIT_
            do 140 ii = 1, dimaki
                if ((comp(1:4).eq.'KIT_') .or. (comp(1:4).eq.'META')) then
                    zk16(jvalv-1+ii+7) = nomkit(ii)
                else
                    zk16(jvalv-1+ii+7) = '        '
                endif
140          continue
            if ((comp(1:5).eq.'KIT_H') .or. (comp(1:6).eq.'KIT_TH')) then
                do 180 inv = 1, dimanv
                    write (zk16(jvalv-1+7+dimaki+inv),'(I16)') nbnvi(&
                    inv)
180              continue
            endif
            if (comp .eq. 'KIT_DDI') then
                if ((nomkit(1)(1:4).eq.'GLRC') .or. (nomkit(2)(1:4) .eq.'GLRC')) then
                    nbvari=nbvari+10
                endif
            endif
!
!         POUR LES COMPORTEMENTS UMAT
!         ON STOCKE LA LIB DANS KIT1-KIT8 (128 CARACTERES)
!         ET LA SUBROUTINE DANS KIT9
            if ((comp.eq.'UMAT') .or. (comp.eq.'MFRONT')) then
                call getvis(moclef, 'NB_VARI', k, iarg, 1,&
                            nbvarz, n1)
                nbvari=nbvarz+nbvari
                call getvtx(moclef, 'LIBRAIRIE', k, iarg, 1,&
                            nomlib, n1)
                call getvtx(moclef, 'NOM_ROUTINE', k, iarg, 1,&
                            nomsub, n1)
                do 30 ii = 1, dimaki-1
                    zk16(jvalv-1+ii+7) = nomlib(16*(ii-1)+1:16*ii)
30              continue
                zk16(jvalv-1+dimaki+7) = nomsub
            endif
!         FIN DES CAS PARTICULIERS
! ======================================================================
!
!         REMPLISSAGE DES CMP
!
            zk16(jvalv-1+1) = comp
            write (zk16(jvalv-1+2),'(I16)') nbvari
            zk16(jvalv-1+3) = defo
            zk16(jvalv-1+4) = moclef(i)
            zk16(jvalv-1+5) = txcp
!         ON ECRIT NUMLC EN POSITION 6 (CMP XXX1)
            if (comp(1:8) .ne. 'MULTIFIB') then
                write (zk16(jvalv-1+6),'(I16)') numlc
            endif
!
            if (nbma .ne. 0) then
                call jeveuo(mesmai, 'L', jma)
                call nocart(compor, 3, k8b, 'NUM', nbma,&
                            k8b, zi(jma), ' ', ncmpma)
                call jedetr(mesmai)
            else
! -----   PAR DEFAUT C'EST TOUT='OUI'
                call nocart(compor, 1, k8b, k8b, 0,&
                            k8b, ibid, k8b, ncmpma)
            endif
!
150      continue
160  end do
!
! ======================================================================
!     SI MULTIFIBRE, ON FUSIONNE AVEC LA CARTE CREEE DANS AFFE_MATERIAU
!      / AFFE_COMPOR - RCCOMP.F
    if (exipmf) then
        call nmdpmf(compor)
    endif
! ======================================================================
170  continue
!
!     SI ZMAT, ON REINITIALISE LES ZASTER_HANDLER POUR FORCER
!     LA RELECTURE DES FICHIERS DECRIVANT LES COMPORTEMENTS
    if (iszmat) then
        call zaswri()
    endif
!
    call jedetr(compor//'.NCMP')
    call jedetr(compor//'.VALV')
! FIN ------------------------------------------------------------------
    indimp=1
    call jedema()
end subroutine
