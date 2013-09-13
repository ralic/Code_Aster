subroutine anacri(nomcri, nomfor, typcha, impgrd, paract,&
                  fordef, crsigm, crepst, crepse, crepsp)
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
!
    implicit none
#include "jeveux.h"
#include "asterfort/fonbpa.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
    character(len=3) :: impgrd
    character(len=16) :: nomcri, nomfor, typcha
    integer :: paract (30)
    logical :: fordef, crsigm, crepst, crepse, crepsp
!
! ---------------------------------------------------------------------
! BUT: ANALYSER LE CRITERE POUR DETERMINER LES GRANDEURS NECESSAIARES
!                                A CALCUCLER
! ---------------------------------------------------------------------
! ARGUMENTS:
! NOMCRI   IN    K16: NOM DU CRITERE D'ENDOMMAGEMENT PAR FATIGUE.
! NOMFOR   IN    K16: LE NOM DE FORMULE DE GRNADUER EQUIVALENTE
! IMPGRD   IN    K3 : 'OUI' : IMPRIMER LES GRANDEURS A CALCULER
!                     'NON':  PAS IMPRIMER
! TYPCHA   IN    K16: TYPE DE CHARGEMENT (PERIODIQUE OU NON).
! PARACT   OUT   REAL: INDICATEUR DU GRANDEUR ACTIVE
!                      PARACT(K) = 1: K-IEME GRANDEUR EST ACTIVE
!
! FORDEF  LOGICAL : 'OUI' POUR LA PROJECTION DE L'HISTOIRE
!       DE DEFORMATION CISSAILLEMEMENT ET 'NON' POUR LA PROJECTION DE
!                   L'HISTOIRE DE CONTRAINET CISSAILLEMEMENT
! CRSIGM  LOGICAL : HISTOIRE DE CONTRAINTE NECESSAIRE
! CREPST  LOGICAL : HISTOIRE DE DEFORMATION TOTALE NECESSAIRE
! CREPSE  LOGICAL : HISTOIRE DE DEFORMATION TOTALE NECESSAIRE
! CREPSP  LOGICAL : HISTOIRE DE DEFORMATION PLASTIQUE NECESSAIRE
!
!-----------------------------------------------------------------------
    integer :: ip, id, nparma, jprof, np, l
    character(len=2) :: nomty1(22), nomty2(30)
    character(len=8) :: nompa1(22), nompa2(30), nompf(30)
    character(len=24) :: chnom, cbid
    logical :: grdexi
!
!     ---------------------------------------------------------------
    data  nompa1/   'DTAUMA', 'PHYDRM', 'NORMAX', 'NORMOY',&
     &                  'EPNMAX', 'EPNMOY', 'DEPSPE', 'EPSPR1',&
     &                  'SIGNM1', 'DENDIS', 'DENDIE', 'APHYDR',&
     &                  'MPHYDR', 'DSIGEQ', 'SIGPR1', 'EPSNM1',&
     &                  'INVA2S', 'DSITRE', 'DEPTRE', 'EPSPAC',&
     &                  'RAYSPH', 'AMPCIS'  /
!     ---------------------------------------------------------------
!     ---------------------------------------------------------------
!      C = CONTRAINTE, T = DEF TOTALE, E = DEF ELAS, P = DEF PLAS
!
    data  nomty1/   'CC', 'CC', 'CC', 'CC',&
     &                  'TT', 'TT', 'PP', 'TT',&
     &                  'CT', 'CP', 'CE', 'CC',&
     &                  'CC', 'CC', 'CC', 'TC',&
     &                  'TT', 'CC', 'TT', 'PP',&
     &                  'CC', 'CC'  /
!     ---------------------------------------------------------------
!     ---------------------------------------------------------------
    data  nompa2/  'TAUPR_1','TAUPR_2','SIGN_1',  'SIGN_2',&
     &                 'PHYDR_1','PHYDR_2','EPSPR_1', 'EPSPR_2',&
     &                 'SIPR1_1','SIPR1_2','EPSN1_1', 'EPSN1_2',&
     &                 'ETPR1_1','ETPR1_2','SITN1_1', 'SITN1_2',&
     &                 'EPPR1_1','EPPR1_2','SIPN1_1', 'SIPN1_2',&
     &                 'SIGEQ_1','SIGEQ_2', 'ETEQ_1', 'ETEQ_2',&
     &                 'EPEQ_1', 'EPEQ_2',  'INVJ2_1','INVJ2_2',&
     &                 'SITRE_1', 'SITRE_2'     /
!       -------------------------------------------------------------
    data  nomty2/  'CC','CC','CC',  'CC',&
     &                 'CC','CC','TT', 'TT',&
     &                 'CC','CC','TC', 'TC',&
     &                 'TT','TT','CT', 'CT',&
     &                 'PP','PP','CP', 'CP',&
     &                 'CC','CC', 'TT', 'TT',&
     &                 'PP','PP', 'TT','TT',&
     &                 'CC', 'CC'     /
!
!
!-----------------------------------------------------------------------
!234567                                                              012
!
    call jemarq()
!
!
! NOMBRE MAX DE PARAMETRES DISPONIBLES
    nparma = 30
!
!     INITIALISATION
    do 15 ip = 1, nparma
        paract(ip) = 0
15  end do
!
    fordef = .false.
    if (nomcri(1:7) .eq. 'FORMULE') then
!
! RECUPERER LES NOMS DE PARAMETRES FOURNIS PAR L'UTILISATEUR
        chnom(20:24) = '.PROL'
        chnom(1:19) = nomfor
!
        call jeveuo(chnom, 'L', jprof)
        call fonbpa(nomfor, zk24(jprof), cbid, nparma, np,&
                    nompf)
!
! VERIFIER QUE LE NOM DE GRANDEUR A CALCULER EST BON
        if (typcha .eq. 'NON_PERIODIQUE') then
            do 10 id = 1, np
                grdexi = .false.
                do 40 ip = 1, nparma
                    if (nompf(id) .eq. nompa2(ip)) then
                        grdexi = .true.
                        paract(ip) = 1
                    endif
40              continue
                if (.not. grdexi) then
                    call utmess('F', 'FATIGUE1_91', sk=nompf(id))
                endif
!
                if (nompf(id)(1:3) .eq. 'EPS') then
                    fordef = .true.
                    do 20 ip = 1, np
                        if (nompf(ip)(1:3) .eq. 'TAU') then
                            call utmess('F', 'FATIGUE1_92')
                        endif
20                  continue
                endif
                if (nompf(id)(1:3) .eq. 'TAU') then
                    do 30 ip = 1, np
                        if (nompf(ip)(1:3) .eq. 'EPS') then
                            call utmess('F', 'FATIGUE1_92')
                        endif
30                  continue
                endif
10          continue
!
        else
!
            do 60 id = 1, np
                grdexi = .false.
                do 50 ip = 1, nparma
                    if (nompf(id) .eq. nompa1(ip)) then
                        grdexi = .true.
                        paract(ip) = 1
                    endif
50              continue
!
                if (.not. grdexi) then
                    call utmess('F', 'FATIGUE1_91', sk=nompf(id))
                endif
!
60          continue
        endif
!
    endif
!
    if (nomcri(1:14) .eq. 'MATAKE_MODI_AC') then
        paract(1) = 1
        paract(3) = 1
        paract(4) = 1
        paract(5) = 1
        paract(6) = 1
    endif
!
    if (nomcri(1:16) .eq. 'DANG_VAN_MODI_AC') then
        paract(1) = 1
        paract(2) = 1
        paract(4) = 1
        paract(5) = 1
        paract(6) = 1
    endif
!
    if (nomcri(1:14) .eq. 'MATAKE_MODI_AV') then
        paract(1) = 1
        paract(2) = 1
        paract(3) = 1
        paract(4) = 1
    endif
!
    if (nomcri(1:16) .eq. 'DANG_VAN_MODI_AV') then
        paract(1) = 1
        paract(2) = 1
        paract(5) = 1
        paract(6) = 1
    endif
!
    if (nomcri(1:16) .eq. 'FATESOCI_MODI_AV') then
        paract(3) = 1
        paract(4) = 1
        paract(7) = 1
        paract(8) = 1
    endif
!
    if (nomcri(1:11) .eq. 'VMIS_TRESCA') then
        paract(14) = 1
        paract(18) = 1
    endif
!
! DANS POST_FATIGUE
    if (nomcri(1:9) .eq. 'CROSSLAND') then
        paract(2) = 1
        paract(22) = 1
    endif
!
    if (nomcri(1:12) .eq. 'PAPADOPOULOS') then
        paract(2) = 1
        paract(21) = 1
    endif
! POUR OPERATEUR POST_FATIGUE
!
! ANALYSER LES HISTORES NECESSAIRE
    crsigm = .false.
    crepst = .false.
    crepse = .false.
    crepsp = .false.
!
    do 80 ip = 1, nparma
        if (paract(ip) .eq. 1) then
            do 81 l = 1, 2
                if (typcha .eq. 'PERIODIQUE') then
                    if (nomty1(ip)(l:l) .eq. 'C') then
                        crsigm = .true.
                    endif
                    if (nomty1(ip)(l:l) .eq. 'T') then
                        crepst = .true.
                    endif
                    if (nomty1(ip)(l:l) .eq. 'E') then
                        crepse = .true.
                    endif
                    if (nomty1(ip)(l:l) .eq. 'P') then
                        crepsp = .true.
                    endif
                else
                    if (nomty2(ip)(l:l) .eq. 'C') then
                        crsigm = .true.
                    endif
                    if (nomty2(ip)(l:l) .eq. 'T') then
                        crepst = .true.
                    endif
                    if (nomty2(ip)(l:l) .eq. 'E') then
                        crepse = .true.
                    endif
                    if (nomty2(ip)(l:l) .eq. 'P') then
                        crepsp = .true.
                    endif
                endif
81          continue
        endif
80  end do
!
! IMPRIMER DES INFO
    if (impgrd .eq. 'OUI') then
        write(6,*)'CRITERE AMORCAGE A UTILISER ==>',nomcri
        write(6,*)' '
        write(6,*)'LES GRANDEURS A CALCULER : '
        do 70 ip = 1, nparma
            if (paract(ip) .eq. 1) then
!
                if (typcha .eq. 'PERIODIQUE') then
                    write(6,*)'    ', nompa1(ip)
                    write(6,*) ' '
                else
                    write(6,*)'    ', nompa2(ip)
                    write(6,*) ' '
                endif
            endif
!
70      continue
!
        write(6,*)'HISTOIRES DE CHARGEMENT DOIVENT CONSISTER :'
!
        if (crsigm) then
            write(6,*) '    CONTRAINTE'
        endif
!
        if (crepst) then
            write(6,*) '    DEFORMATION TOTALE'
        endif
!
        if (crepse) then
            write(6,*) '    DEFORMATION ELASTIQUE'
        endif
!
        if (crepsp) then
            write(6,*) '    DEFORMATION PLASTIQUE'
        endif
        write(6,*) ' '
!
        if (crepse) then
            write(6,*) 'ON NOTE: DEFORMATION ELASTIQUE = DEFORMATION'//&
                &'TOTALE - DEFORMATION PLASTIQUE'
            if (.not. crepst) then
                write(6,*) 'LE CHARGEMENT DOIT CONSISTER EN PLUS:'//&
                    &'DEFORMATION TOTALE (OBLIGATOIRE)'
            endif
!
            if (.not. crepsp) then
                write(6,*) 'LE CHARGEMENT DOIT CONSISTER EN PLUS:'//&
                    &'DEFORMATION PLASTIQUE (OPTIONEL)'
            endif
!
        endif
    endif
!
    call jedema()
end subroutine
