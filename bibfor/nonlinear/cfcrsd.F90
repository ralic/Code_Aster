subroutine cfcrsd(noma, numedd, defico, resoco)
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
    implicit     none
#include "jeveux.h"
!
#include "asterc/r8vide.h"
#include "asterfort/cfcrje.h"
#include "asterfort/cfcrli.h"
#include "asterfort/cfcrma.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/infdbg.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jexnum.h"
#include "asterfort/vtcreb.h"
#include "asterfort/wkvect.h"
    character(len=8) :: noma
    character(len=24) :: numedd
    character(len=24) :: defico, resoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES)
!
! CREATION DES STRUCTURES DE DONNEES NECESSAIRES AU TRAITEMENT
! DU CONTACT/FROTTEMENT
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  NUMEDD : NOM DU NUME_DDL
! IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
! OUT RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
!
!
!
!
    integer :: ifm, niv
    integer :: ztacf
    integer :: ndimg, ntpc, nnoco, neq
    integer :: nbliai, ii
    character(len=8) :: k8bid
    character(len=19) :: mu, atmu, afmu, copo
    integer :: jmu, jatmu, jafmu, jcopo
    character(len=19) :: ddepl0, ddeplc, ddelt, depl0, deplc
    character(len=19) :: cm1a, enat, fro1, fro2
    character(len=19) :: secmbr, cncin0
    character(len=19) :: sgradm, sgradp, sgrprm, sgrprp, direct, mum, svmu
    integer :: jsgram, jsgrap, jsgprm, jsgprp, jdirec, jmum, jsvmu
    character(len=19) :: pcresi, pcdire, pcdepl
    integer :: jpcres, jpcdir, jpcdep
    integer :: nbcm1a, nbenat, nbfro1, nbfro2
    character(len=24) :: autoc1, autoc2
    character(len=24) :: clreac, tacfin, tangco
    integer :: jclrea, jtacf, jtango
    logical :: lctfd, lpenac, lpenaf, lmatrc, lgcp, lctf3d, ldiric
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... CREATION DE LA SD RESULTAT'//&
        ' CONTACT DISCRET'
    endif
!
! --- INFOS SUR LA CHARGE DE CONTACT
!
    lctfd = cfdisl(defico,'FROT_DISCRET' )
    lpenac = cfdisl(defico,'CONT_PENA' )
    lpenaf = cfdisl(defico,'FROT_PENA' )
    lmatrc = cfdisl(defico,'MATR_CONT' )
    lgcp = cfdisl(defico,'CONT_GCP' )
    lctf3d = cfdisl(defico,'FROT_3D' )
    ldiric = cfdisl(defico,'PRE_COND_DIRICHLET')
!
! --- SD POUR APPARIEMENT
!
    ztacf = cfmmvd('ZTACF')
!
! --- INFORMATIONS
!
    ndimg = cfdisi(defico,'NDIM' )
    ntpc = cfdisi(defico,'NTPC' )
    nnoco = cfdisi(defico,'NNOCO')
!
! --- PARAMETRES DE REACTUALISATION GEOMETRIQUE
! CLREAC(1) = TRUE  SI REACTUALISATION A FAIRE
! CLREAC(2) = TRUE  SI ATTENTE POINT FIXE CONTACT
! CLREAC(3) = TRUE  SI PREMIERE REACTUALISATION DU PAS DE TEMPS
!
    autoc1 = resoco(1:14)//'.REA1'
    autoc2 = resoco(1:14)//'.REA2'
    call vtcreb(autoc1, numedd, 'V', 'R', neq)
    call vtcreb(autoc2, numedd, 'V', 'R', neq)
    clreac = resoco(1:14)//'.REAL'
    call wkvect(clreac, 'V V L', 4, jclrea)
    zl(jclrea+1-1) = .false.
    zl(jclrea+2-1) = .false.
    zl(jclrea+3-1) = .false.
    zl(jclrea+4-1) = .false.
!
! --- INFORMATIONS POUR METHODES "PENALISATION" ET "LAGRANGIEN"
!
    tacfin = resoco(1:14)//'.TACFIN'
    call wkvect(tacfin, 'V V R', ntpc*ztacf, jtacf)
!
! --- TANGENTES RESULTANTES
!
    tangco = resoco(1:14)//'.TANGCO'
    call wkvect(tangco, 'V V R', 6*ntpc, jtango)
!
! --- SD POUR LES JEUX
!
    call cfcrje(defico, resoco)
!
! --- SD POUR LES LIAISONS LINEAIRES
!
    call cfcrli(noma, numedd, defico, resoco)
!
! --- LAGRANGES DE CONTACT/FROTTEMENT
!
    mu = resoco(1:14)//'.MU'
    call wkvect(mu, 'V V R', 4*ntpc, jmu)
!
! --- VALEUR DE LA PSEUDO-PENALISATION EN FROT. LAGR.
!
    copo = resoco(1:14)//'.COPO'
    call wkvect(copo, 'V V R', 1, jcopo)
    zr(jcopo) = r8vide()
!
! --- FORCES NODALES DE CONTACT
!
    atmu = resoco(1:14)//'.ATMU'
    call wkvect(atmu, 'V V R', neq, jatmu)
!
! --- FORCES NODALES DE FROTTEMENT
!
    if (lctfd) then
        afmu = resoco(1:14)//'.AFMU'
        call wkvect(afmu, 'V V R', neq, jafmu)
    endif
!
! --- FORCES NODALES DE CONTACT DANS LE CAS DE LA METHODE PENALISEE
! --- ON UTILISE AFMU
!
    if (lpenac .and. (.not.lctfd)) then
        afmu = resoco(1:14)//'.AFMU'
        call wkvect(afmu, 'V V R', neq, jafmu)
    endif
!
! --- INCREMENT DE SOLUTION SANS CORRECTION DU CONTACT
!
    ddepl0 = resoco(1:14)//'.DEL0'
    call vtcreb(ddepl0, numedd, 'V', 'R', neq)
!
! --- INCREMENT DE SOLUTION ITERATION DE CONTACT
!
    ddelt = resoco(1:14)//'.DDEL'
    call vtcreb(ddelt, numedd, 'V', 'R', neq)
!
! --- INCREMENT DE SOLUTION APRES CORRECTION DU CONTACT
!
    ddeplc = resoco(1:14)//'.DELC'
    call vtcreb(ddeplc, numedd, 'V', 'R', neq)
!
! --- INCREMENT DE DEPLACEMENT CUMULE DEPUIS DEBUT DU PAS DE TEMPS
! --- SANS CORRECTION DU CONTACT
!
    if (lctfd) then
        depl0 = resoco(1:14)//'.DEP0'
        call vtcreb(depl0, numedd, 'V', 'R', neq)
    endif
!
! --- INCREMENT DE DEPLACEMENT CUMULE DEPUIS DEBUT DU PAS DE TEMPS
! --- AVEC CORRECTION DU CONTACT
!
    if (lctfd) then
        deplc = resoco(1:14)//'.DEPC'
        call vtcreb(deplc, numedd, 'V', 'R', neq)
    endif
!
! --- CHARGEMENT CINEMATIQUE NUL
!
    cncin0 = resoco(1:14)//'.CIN0'
    call vtcreb(cncin0, numedd, 'V', 'R', neq)
!
! --- CHAMPS POUR GCP
!
    if (lgcp) then
        sgradm = resoco(1:14)//'.SGDM'
        sgradp = resoco(1:14)//'.SGDP'
        direct = resoco(1:14)//'.DIRE'
        sgrprm = resoco(1:14)//'.SGPM'
        sgrprp = resoco(1:14)//'.SGPP'
        mum = resoco(1:14)//'.MUM'
        secmbr = resoco(1:14)//'.SECM'
        call vtcreb(secmbr, numedd, 'V', 'R', neq)
        call wkvect(sgradm, 'V V R', ntpc, jsgram)
        call wkvect(sgradp, 'V V R', ntpc, jsgrap)
        call wkvect(sgrprm, 'V V R', ntpc, jsgprm)
        call wkvect(sgrprp, 'V V R', ntpc, jsgprp)
        call wkvect(direct, 'V V R', ntpc, jdirec)
        call wkvect(mum, 'V V R', ntpc, jmum)
        if (ldiric) then
            pcresi = resoco(1:14)//'.PCRS'
            call wkvect(pcresi, 'V V R', ntpc, jpcres)
            pcdire = resoco(1:14)//'.PCDR'
            call wkvect(pcdire, 'V V R', ntpc, jpcdir)
            pcdepl = resoco(1:14)//'.PCUU'
            call wkvect(pcdepl, 'V V R', neq, jpcdep)
        endif
    endif
!
! --- OBJET DE SAUVEGARDE DU LAGRANGE DE CONTACT EN GCP
!
    if (lgcp) then
!       ETAT CONVERGE
        svmu = resoco(1:14)//'.SVM0'
        call wkvect(svmu, 'V V R', nnoco, jsvmu)
!       ETAT COURANT AVANT APPARIEMENT
        svmu = resoco(1:14)//'.SVMU'
        call wkvect(svmu, 'V V R', nnoco, jsvmu)
    endif
!
! --- SD DE DONNEES POUR LES MATRICES DE CONTACT
!
    if (.not.lgcp) then
        nbliai = ntpc
!
! ---   DETERMINATION DES TAILLES DES DIFFERENTES MATRICES
!
        nbenat = 0
        nbcm1a = 0
        nbfro1 = 0
        nbfro2 = 0
        if (lpenac) then
!         PENALISATION DU CONTACT
!         ENAT
            nbenat = nbliai
        else if ((.not.lctfd) .or. lpenaf) then
!         DUALISATION DU CONTACT SEULEMENT
!         CM1A
            nbcm1a = nbliai
        else
!         DUALISATION DU CONTACT ET DU FROTTEMENT
!         CM1A
            nbcm1a = ndimg*nbliai
        endif
!
!       FRO1
        nbfro1 = (ndimg-1)*nbliai
!
!       FRO2
        nbfro2 = nbliai
!
!
! ---   CREATIONS DES MATRICES
!
        if (lpenac) then
!
! ---   MATRICE STOCKEE CREUSE E_N*AT (POUR CONTACT PENALISE)
! ---   TAILLE : NBENAT*30
!
            enat = resoco(1:14)//'.ENAT'
            call jecrec(enat, 'V V R', 'NU', 'DISPERSE', 'CONSTANT',&
                        nbenat)
            call jeecra(enat, 'LONMAX', 30, k8bid)
            do 30 ii = 1, nbenat
                call jecroc(jexnum(enat, ii))
30          continue
        else
!
! ---   MATRICE PRINCIPALE C-1*AT (POUR CONTACT DUALISE)
! ---   TAILLE : NBCM1A*NEQ
!
            cm1a = resoco(1:14)//'.CM1A'
            call jecrec(cm1a, 'V V R', 'NU', 'DISPERSE', 'CONSTANT',&
                        nbcm1a)
            call jeecra(cm1a, 'LONMAX', neq, k8bid)
            do 40 ii = 1, nbcm1a
                call jecroc(jexnum(cm1a, ii))
40          continue
!
        endif
!
! ---   MATRICES STOCKEES CREUSES FRO1 ET FRO2
! ---   (POUR FROTTEMENT UNIQUEMENT)
! ---   TAILLE : NBFRO1*30 ET NBFRO2*30
!
        if (lctf3d) then
            fro1 = resoco(1:14)//'.FRO1'
            fro2 = resoco(1:14)//'.FRO2'
            call jecrec(fro1, 'V V R', 'NU', 'DISPERSE', 'CONSTANT',&
                        nbfro1)
            call jecrec(fro2, 'V V R', 'NU', 'DISPERSE', 'CONSTANT',&
                        nbfro2)
            call jeecra(fro1, 'LONMAX', 30, k8bid)
            call jeecra(fro2, 'LONMAX', 30, k8bid)
            do 41 ii = 1, nbfro1
                call jecroc(jexnum(fro1, ii))
41          continue
            do 42 ii = 1, nbfro2
                call jecroc(jexnum(fro2, ii))
42          continue
        endif
    endif
!
! --- MATRICE DE CONTACT ACM1AT
!
    if (lmatrc) then
        call cfcrma(nbcm1a, noma, resoco)
    endif
!
    call jedema()
end subroutine
