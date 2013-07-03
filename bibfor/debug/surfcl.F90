subroutine surfcl(char, noma, ifm)
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
    implicit    none
#include "jeveux.h"
!
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfl.h"
#include "asterfort/mminfr.h"
#include "asterfort/surfll.h"
#include "asterfort/wkvect.h"
    character(len=8) :: char
    character(len=8) :: noma
    integer :: ifm
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - AFFICHAGE DONNEES)
!
! AFFICHAGE LES INFOS CONTENUES DANS LA SD CONTACT POUR LES FORMULATIONS
! MAILLEES (DISCRETE ET CONTINUE MAIS PAS XFEM)
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  NOMA   : NOM DU MAILLAGE
! IN  IFM    : UNITE D'IMPRESSION
!
! ----------------------------------------------------------------------
!
    integer :: zdirn
    integer :: ndim, nzoco, nsuco, nmaco
    integer :: nnoco, nmano, nnoma
    integer :: ntnoe, ntmae, ntnoec, ntmaec
    integer :: ntnom, ntmam, ntnomc, ntmamc
    integer :: ntpc, ntpt
    integer :: izone, k, nsno, ino, jdec, numnoe
!
    character(len=24) :: defico
!
    character(len=8) :: chain1, chain2, k8bid
!
    character(len=24) :: pzone, psurma, psurno, contma, contno
    integer :: jzone, jsuma, jsuno, jmaco, jnoco
    character(len=24) :: manoco, pmano, nomaco, pnoma
    integer :: jmano, jpoma, jnoma, jpono
    character(len=24) :: psans, sansno
    integer :: jpsans, jsans
    character(len=24) :: noeuma
    integer :: jnomno
!
    character(len=24) :: dirapp, dirnor
    integer :: jdirap, jdirno
    character(len=24) :: jeufo1, jeufo2
    integer :: jjfo1, jjfo2
!
    integer :: inorm, itypa, ivecm, ivece, iappa
    logical :: lcoqu, lpout, lveri
    logical :: lliss, lexiv, lstop
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    defico = char(1:8)//'.CONTACT'
!
! --- ACCES SD DU MAILLAGE
!
    noeuma = noma(1:8)//'.NOMNOE'
!
! --- COMMUNS AVEC FORM. MAILLEES (DISCRET ET CONTINUE MAIS PAS XFEM)
!
    pzone = defico(1:16)//'.PZONECO'
    psurma = defico(1:16)//'.PSUMACO'
    psurno = defico(1:16)//'.PSUNOCO'
    contma = defico(1:16)//'.MAILCO'
    contno = defico(1:16)//'.NOEUCO'
    manoco = defico(1:16)//'.MANOCO'
    pmano = defico(1:16)//'.PMANOCO'
    nomaco = defico(1:16)//'.NOMACO'
    pnoma = defico(1:16)//'.PNOMACO'
    sansno = defico(1:16)//'.SSNOCO'
    psans = defico(1:16)//'.PSSNOCO'
    jeufo1 = defico(1:16)//'.JFO1CO'
    jeufo2 = defico(1:16)//'.JFO2CO'
    dirapp = defico(1:16)//'.DIRAPP'
    dirnor = defico(1:16)//'.DIRNOR'
!
    call jeveuo(pzone, 'L', jzone)
    call jeveuo(psurma, 'L', jsuma)
    call jeveuo(psurno, 'L', jsuno)
    call jeveuo(contma, 'L', jmaco)
    call jeveuo(contno, 'L', jnoco)
    call jeveuo(manoco, 'L', jmano)
    call jeveuo(pmano, 'L', jpoma)
    call jeveuo(nomaco, 'L', jnoma)
    call jeveuo(pnoma, 'L', jpono)
    call jeveuo(psans, 'L', jpsans)
    call jeveuo(sansno, 'L', jsans)
    call jeveuo(jeufo1, 'L', jjfo1)
    call jeveuo(jeufo2, 'L', jjfo2)
    call jeveuo(dirapp, 'L', jdirap)
    call jeveuo(dirnor, 'L', jdirno)
!
    zdirn = cfmmvd('ZDIRN')
!
! --- INITIALISATIONS
!
    ndim = cfdisi(defico,'NDIM' )
    nzoco = cfdisi(defico,'NZOCO' )
    nsuco = cfdisi(defico,'NSUCO' )
    nmaco = cfdisi(defico,'NMACO' )
    nnoco = cfdisi(defico,'NNOCO' )
    ntmae = cfdisi(defico,'NTMAE' )
    ntnoe = cfdisi(defico,'NTNOE' )
    ntmaec = cfdisi(defico,'NTMAEC')
    ntnoec = cfdisi(defico,'NTNOEC')
    ntmam = cfdisi(defico,'NTMAM' )
    ntnom = cfdisi(defico,'NTNOM' )
    ntmamc = cfdisi(defico,'NTMAMC')
    ntnomc = cfdisi(defico,'NTNOMC')
    ntpc = cfdisi(defico,'NTPC' )
    ntpt = cfdisi(defico,'NTPT' )
!
! --- CREATION VECTEURS TEMPORAIRES
!
    call wkvect('&&SURFCL.TRAVNO', 'V V K8', nzoco*nnoco, jnomno)
!
! --- IMPRESSIONS POUR L'UTILISATEUR
!
    write (ifm,*)
    write (ifm,*) '<CONTACT> INFOS GENERALES SUR LES FORMULATIONS'//&
     &              ' MAILLEES'
    write (ifm,*)
!
! --- PARAMETRES GENERAUX D'APPARIEMENT
!
    write (ifm,*) '<CONTACT> ... PARAMETRES GENERAUX APPARIEMENT'
!
! --- LISSAGE OU PAS
!
    lliss = cfdisl(defico,'LISSAGE')
    if (lliss) then
        write (ifm,*) '<CONTACT> ...... NORMALES LISSEES'
    endif
!
    do 9 izone = 1, nzoco
        inorm = mminfi(defico,'NORMALE' ,izone )
        iappa = mminfi(defico,'APPARIEMENT',izone )
        itypa = mminfi(defico,'TYPE_APPA' ,izone )
        ivecm = mminfi(defico,'VECT_MAIT' ,izone )
        ivece = mminfi(defico,'VECT_ESCL' ,izone )
!
        write (ifm,*) '<CONTACT> ...... OPTIONS SUR LA ZONE : ',izone
!
        write (ifm,1070) 'APPARIEMENT     ',iappa
        write (ifm,1070) 'NORMALE         ',inorm
        write (ifm,1070) 'TYPE_APPA       ',itypa
!
        if (itypa .eq. 1) then
            write (ifm,1072) 'DIRE_APPA       ',&
     &          zr(jdirap+3*(izone-1)),&
     &          zr(jdirap+3*(izone-1)+1),&
     &          zr(jdirap+3*(izone-1)+2)
        endif
!
! ----- ORIENTATION NORMALES
!
        write (ifm,1070) 'VECT_MAIT       ',ivecm
!
        if (ivecm .eq. 1) then
            write (ifm,1072) 'MAIT_FIXE       ',&
     &          zr(jdirno+zdirn*(izone-1)),&
     &          zr(jdirno+zdirn*(izone-1)+1),&
     &          zr(jdirno+zdirn*(izone-1)+2)
        endif
        if (ivecm .eq. 2) then
            write (ifm,1072) 'MAIT_VECT_Y     ',&
     &          zr(jdirno+zdirn*(izone-1)),&
     &          zr(jdirno+zdirn*(izone-1)+1),&
     &          zr(jdirno+zdirn*(izone-1)+2)
        endif
        write (ifm,1070) 'VECT_ESCL       ',ivece
        if (ivece .eq. 1) then
            write (ifm,1072) 'ESCL_FIXE       ',&
     &          zr(jdirno+zdirn*(izone-1)+3),&
     &          zr(jdirno+zdirn*(izone-1)+4),&
     &          zr(jdirno+zdirn*(izone-1)+5)
        endif
        if (ivece .eq. 2) then
            write (ifm,1072) 'ESCL_VECT_Y     ',&
     &          zr(jdirno+zdirn*(izone-1)+3),&
     &          zr(jdirno+zdirn*(izone-1)+4),&
     &          zr(jdirno+zdirn*(izone-1)+5)
        endif
!
! ----- TOLERANCES
!
        write (ifm,1071) 'TOLE_PROJ_EXT   ',&
     &                    mminfr(defico,'TOLE_PROJ_EXT',izone )
        write (ifm,1071) 'TOLE_APPA       ',&
     &                    mminfr(defico,'TOLE_APPA'    ,izone )
!
! ----- IMPRESSIONS SANS_GROUP_NO
!
        write (ifm,*) '<CONTACT> ...... NOEUDS EXCLUS '
!
        nsno = zi(jpsans+izone) - zi(jpsans+izone-1)
        if (nsno .eq. 0) then
            write (ifm,*) '<CONTACT> ...... PAS DE NOEUDS DE TYPE '//&
            'SANS_GROUP_NO A EXCLURE'
        else
            write (ifm,*) '<CONTACT> ...... NOMBRE DE NOEUDS DE TYPE '//&
     &                 'SANS_GROUP_NO A EXCLURE: ',nsno
!
            jdec = zi(jpsans+izone-1)
            do 801 ino = 1, nsno
                numnoe = zi(jsans+jdec+ino-1)
                call jenuno(jexnum(noeuma, numnoe), zk8(jnomno+ino-1))
801          continue
!
            write (ifm,1040) '     LISTE DES NOEUDS  : '
            write (ifm,1050) (zk8(jnomno+ino-1), ino = 1,nsno)
        endif
!
 9  end do
!
    1040 format (' <CONTACT> ...... ',a25)
    1050 format ((' <CONTACT> ...... ',17x,4(a8,1x)))
    1070 format (' <CONTACT> ...... PARAM. : ',a16,' - VAL. : ',i5)
    1071 format (' <CONTACT> ...... PARAM. : ',a16,' - VAL. : ',e12.5)
    1072 format (' <CONTACT> ...... PARMA. : ',a16,' - VAL. : ',e12.5,&
     &        e12.5,e12.5)
!
! --- PARAMETRES JEUX SUPPS
!
    write (ifm,*) '<CONTACT> ... JEUX SUPPLEMENTAIRES'
!
    do 21 izone = 1, nzoco
        lcoqu = mminfl(defico,'DIST_COQUE' ,izone )
        lpout = mminfl(defico,'DIST_POUTRE',izone )
        chain1 = zk8(jjfo1+izone-1)
        chain2 = zk8(jjfo2+izone-1)
!
        if ((chain1.ne.' ') .or. (chain2.ne.' ')) then
            write (ifm,*) '<CONTACT> ... JEU SUPP. SUR ZONE ',izone
        endif
        if (chain1 .ne. ' ') then
            write (ifm,1031) chain1
        endif
        if (chain2 .ne. ' ') then
            write (ifm,1032) chain2
        endif
!
! --- IMPRESSIONS POUR LES POUTRES
!
        if (lpout) then
            write (ifm,*) '<CONTACT> ...... JEU SUPP. PAR DIST_POUT'
        endif
!
! --- IMPRESSIONS POUR LES COQUES
!
        if (lcoqu) then
            write (ifm,*) '<CONTACT> ...... JEU SUPP. PAR DIST_COQUE'
        endif
21  end do
!
    1031 format (' <CONTACT> ...... JEU SUPP. TYPE FONC. SUR ESCLAVE : ',&
     &        a8)
    1032 format (' <CONTACT> ...... JEU SUPP. TYPE FONC. SUR MAITRE  : ',&
     &        a8)
!
! --- CALCUL DE CONTACT OU PAS
!
    lexiv = cfdisl(defico,'EXIS_VERIF')
    if (lexiv) then
        lstop = cfdisl(defico,'STOP_INTERP')
        write (ifm,*) '<CONTACT> ... ZONES SANS RESOLUTION DU CONTACT'
        if (lstop) then
            write (ifm,*) '<CONTACT> ...... STOP SI INTERPENETRATION'
        else
            write (ifm,*) '<CONTACT> ...... ALARME SI INTERPENETRATION'
        endif
        do 31 izone = 1, nzoco
            lveri = mminfl(defico,'VERIF' ,izone )
            write (ifm,*) '<CONTACT> ...... OPTIONS SUR LA ZONE : ',&
            izone
            if (lveri) then
                write (ifm,*) '<CONTACT> ...... RESOLUTION DU CONTACT'
            else
                write (ifm,*) '<CONTACT> ...... PAS DE RESOLUTION DU CONTACT'
                write (ifm,1071) 'TOLE_INTERP   ',&
     &                    mminfr(defico,'TOLE_INTERP',izone )
            endif
31      continue
    endif
!
! --- IMPRESSIONS GLOBALES (TOUTES ZONES)
!
    write (ifm,*)
    write (ifm,*) '<CONTACT> INFOS SPECIFIQUES SUR LES FORMULATIONS'//&
     &              ' MAILLEES'
    write (ifm,*)
!
    write (ifm,*) '<CONTACT> DIMENSION DE L''ESPACE   : ',&
     &               ndim
    write (ifm,*) '<CONTACT> NBRE ZONES DE CONTACT   : ',&
     &               nzoco
    write (ifm,*) '<CONTACT> NBRE SURF. DE CONTACT   : ',&
     &               nsuco
    write (ifm,*) '<CONTACT> NBRE MAIL. DE CONTACT   : ',&
     &               nmaco
    write (ifm,*) '<CONTACT> NBRE NOEU. DE CONTACT   : ',&
     &               nnoco
    write (ifm,*) '<CONTACT> NBRE NOEU. ESCL. TOTAL  : ',&
     &               ntnoe,' DONT CALCUL ',ntnoec
    write (ifm,*) '<CONTACT> NBRE MAIL. ESCL. TOTAL  : ',&
     &               ntmae,' DONT CALCUL ',ntmaec
    write (ifm,*) '<CONTACT> NBRE NOEU. MAIT. TOTAL  : ',&
     &               ntnom,' DONT CALCUL ',ntnomc
    write (ifm,*) '<CONTACT> NBRE MAIL. MAIT. TOTAL  : ',&
     &               ntmam,' DONT CALCUL ',ntmamc
!
    write (ifm,*) '<CONTACT> NBRE PT. CONTACT TOTAL  : ',&
     &               ntpt,' DONT CALCUL ',ntpc
!
! --- NOEUDS/MAILLES DES SURFACES
!
    call surfll(defico, noma, ifm, nzoco, nmaco,&
                nnoco)
!
! --- IMPRESSIONS GLOBALES (TOUTES ZONES)
!
    write (ifm,*)
    write (ifm,*) '<CONTACT> INFOS SPECIFIQUES SUR LES FORMULATIONS'//&
     &              ' MAILLEES - NIVEAU DEVELOPPEUR'
    write (ifm,*)
!
    write (ifm,1080) 'PZONE  : '
    write (ifm,1060) (zi(jzone+k), k = 0,nzoco)
!
    write (ifm,1080) 'PSURMA : '
    write (ifm,1060) (zi(jsuma+k), k = 0,nsuco)
!
    write (ifm,1080) 'PSURNO : '
    write (ifm,1060) (zi(jsuno+k), k = 0,nsuco)
!
    write (ifm,1080) 'CONTMA : '
    write (ifm,1060) (zi(jmaco+k-1), k = 1,nmaco)
!
    write (ifm,1080) 'CONTNO : '
    write (ifm,1060) (zi(jnoco+k-1), k = 1,nnoco)
!
    1060 format ((' <CONTACT> ',9x,6(i7,1x)))
    1080 format (' <CONTACT> ',a9)
!
    write (ifm,*)
    write (ifm,*) '<CONTACT> INFOS SPECIFIQUES SUR LES FORMULATIONS'//&
     &              ' MAILLEES - CONNECTIVITES INVERSES'
    write (ifm,*)
    call jelira(manoco, 'LONUTI', nmano, k8bid)
    call jelira(nomaco, 'LONUTI', nnoma, k8bid)
!
!
!
    write (ifm,*) '<CONTACT> NBRE CONNEC. INV        : ',&
     &               nmano
    write (ifm,*) '<CONTACT> NNOMA                   : ',&
     &               nnoma
!
    write (ifm,2080) 'MANOCO : '
    write (ifm,2060) (zi(jmano+k-1), k = 1,nmano)
    write (ifm,2080) 'PMANO  : '
    write (ifm,2060) (zi(jpoma+k), k = 0,nnoco)
    write (ifm,2080) 'NOMACO : '
    write (ifm,2060) (zi(jnoma+k-1), k = 1,nnoma)
    write (ifm,2080) 'PNOMA  : '
    write (ifm,2060) (zi(jpono+k), k = 0,nmaco)
    write (ifm,*)
!
    2060 format ((' <CONTACT> ',9x,6(i7,1x)))
    2080 format (' <CONTACT> ',a9)
!
! --- MENAGE
!
    call jedetr('&&SURFCL.TRAVNO')
!
    call jedema()
end subroutine
