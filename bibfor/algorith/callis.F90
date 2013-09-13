subroutine callis(nomres)
!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
    implicit none
!***********************************************************************
!    P. RICHARD     DATE 13/10/92
!-----------------------------------------------------------------------
!  BUT:      < CALCUL DES LIAISONS >
!
!  CALCULER LES NOUVELLES MATRICE DE LIAISON EN TENANT COMPTE
!   DE L'ORIENTATION DES SOUS-STRUCTURES
!  ON DETERMINE LES MATRICE DE LIAISON, LES DIMENSIONS DE CES MATRICES
!  ET LE PRONO ASSOCIE
!
!  VERIFICATION DE LA COHERENCE DES INTERFACE EN VIS-A-VIS
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
! NOMRES   /I/: NOM UTILISATEUR DU RESULTAT
!
!
!
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvtx.h"
#include "asterfort/inclis.h"
#include "asterfort/inilag.h"
#include "asterfort/jecrec.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/liacar.h"
#include "asterfort/liared.h"
#include "asterfort/lipsrb.h"
#include "asterfort/mgutdm.h"
#include "asterfort/prjlis.h"
#include "asterfort/rotlir.h"
#include "asterfort/rotlis.h"
#include "asterfort/utmess.h"
#include "asterfort/vecomo.h"
#include "asterfort/verili.h"
#include "asterfort/wkvect.h"
!
!
!
    character(len=8) :: nomres, option
    character(len=24) :: famli, fmlia, promli
    character(len=24) :: fpli1o, fpli2o, fpli1n, fpli2n, int1, int2, indin1
    character(len=24) :: indin2, lino1, lino2, tramo1, tramo2, indcol
    character(len=8) :: sst1, sst2, intf1, intf2, mod1, mod2, lint1, lint2
    character(len=8) :: k8bid, ma1, ma2, matprj
    character(len=16) :: motcle(2)
    integer :: nblis, ldpmli, nbbloc, lllia, iad, nblig, i, iret, ibid, nbno1
    integer :: nbno2, ier, llint1, llint2, iinc, irep11, irep12, irep21, irep22
    integer :: iopt, nbeq1, nbeq2, ddla1, ddla2, imast, nbcol
    integer :: taille(2), icar(4)
    real(kind=8) :: un, moins1
!
!
!-----------------------------------------------------------------------
    data un,moins1 /1.0d+00,-1.0d+00/
!-----------------------------------------------------------------------
!
    call jemarq()
!
!   NOM FAMILLE DEFINITION DES LIAISONS
!
!--------------NOMRES EST LE NOM DU MODELE GENERALISE
    famli=nomres//'      .MODG.LIDF'
    fmlia=nomres//'      .MODG.LIMA'
    promli=nomres//'      .MODG.LIPR'
    matprj='MATPROJ '
!
!  MINI-PROFNO DES LIAISON ORIENTEES ET NON ORIENTEE
!
    fpli1o='&&'//'PGC.PROF.LI1O'
    fpli2o='&&'//'PGC.PROF.LI2O'
    fpli1n='&&'//'PGC.PROF.LI1N'
    fpli2n='&&'//'PGC.PROF.LI2N'
!
!-----------------RECUPERATION DU NOMBRE DE LIAISONS--------------------
!             ET DECLARATION DES FAMILLES
!
    call jelira(famli, 'NMAXOC', nblis)
!
    call wkvect(promli, 'G V IS', nblis*9, ldpmli)
!
!  NOM FAMILLE VOLATILE POUR PROFNO MATRICES ORIENTEES
!
    call jecrec(fpli1n, 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                nblis)
    call jecrec(fpli2n, 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                nblis)
    call jecrec(fpli1o, 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                nblis)
    call jecrec(fpli2o, 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                nblis)
!
!------------------------BOUCLE SUR LES LIAISON-------------------------
!    POUR COMPTAGE BLOC ET STOCKAGE DIMENSION ET AUTRE
!
!
!  FAMILLE A CREER POUR MATRICE LIAISON ORIENTEES
!
    call jecrec(fmlia, 'G V R', 'NU', 'DISPERSE', 'VARIABLE',&
                nblis*3)
!
    nbbloc=0
!
    do 10 i = 1, nblis
!
!   *******************************************
!  RECUPERATION DES DONNEES SOUS-STRUCTURES
!   *******************************************
!
! ------------- LA DEFINITION DE LA LIAISON
        call jeveuo(jexnum(famli, i), 'L', lllia)
        sst1=zk8(lllia)
        intf1=zk8(lllia+1)
        sst2=zk8(lllia+2)
        intf2=zk8(lllia+3)
!
! ------------- ON VERIFIE SI MODES REDUITS OU PAS
        call getvtx('LIAISON', 'OPTION', iocc=i, scal=option, nbret=iopt)
!------------------------------------------C
!--                                      --C
!-- CONSTRUCTION DES MATRICES DE LIAISON --C
!--                                      --C
!------------------------------------------C
!
!
!-------------LE NOM DES MODELES
!
        call mgutdm(nomres, sst1, ibid, 'NOM_MODELE  ', ibid,&
                    mod1)
        call mgutdm(nomres, sst2, ibid, 'NOM_MODELE  ', ibid,&
                    mod2)
!
!-------------LE NOM DES MAILLAGES
        call dismoi('F', 'NOM_MAILLA', mod1, 'MODELE', ibid,&
                    ma1, ier)
        call dismoi('F', 'NOM_MAILLA', mod2, 'MODELE', ibid,&
                    ma2, ier)
!
!--------------LES INTERFACES AMONT DES SOUS-STRUCTURES
        call mgutdm(nomres, sst1, ibid, 'NOM_LIST_INTERF', ibid,&
                    lint1)
        call mgutdm(nomres, sst2, ibid, 'NOM_LIST_INTERF', ibid,&
                    lint2)
!
!--------------LES NOMBRES DES NOEUDS DES INTERFACES
        int1=lint1//'.IDC_LINO'
        call jenonu(jexnom(int1(1:13)//'NOMS', intf1), ibid)
        call jelira(jexnum(int1, ibid), 'LONMAX', nbno1)
!
        int2=lint2//'.IDC_LINO'
        call jenonu(jexnom(int2(1:13)//'NOMS', intf2), ibid)
        call jelira(jexnum(int2, ibid), 'LONMAX', nbno2)
!
!--------------LES LISTES DES NUMEROS DES NOEUDS DES INTERFACES
        call jenonu(jexnom(lint1 //'.IDC_NOMS', intf1), ibid)
        call jeveuo(jexnum(lint1 //'.IDC_LINO', ibid), 'L', llint1)
!
        call jenonu(jexnom(lint2 //'.IDC_NOMS', intf2), ibid)
        call jeveuo(jexnum(lint2 //'.IDC_LINO', ibid), 'L', llint2)
!
!
        if (option .eq. 'CLASSIQU') then
!
!  *******************************************
!  CALCUL DES MATRICES DE LIAISON
!  *******************************************
!
!  MATRICE DE LIAISON 1
!
            iad=ldpmli+(i-1)*9
            call liacar(nomres, sst1, intf1, fpli1n, fpli1o,&
                        i, zi(iad))
            zi(ldpmli+(i-1)*9+2)=nbbloc+1
            nbbloc=nbbloc+1
!
!  MATRICE DE LIAISON 2
!
            iad=ldpmli+(i-1)*9+3
            call liacar(nomres, sst2, intf2, fpli2n, fpli2o,&
                        i, zi(iad))
            zi(ldpmli+(i-1)*9+5)=nbbloc+1
            nbbloc=nbbloc+1
!
!  *******************************************
!  RECUPERATION DES DONNEES INCOMPATIBILITE
!  *******************************************
!
!-- LA GESTION DE L'INCOMPATIBILITE SE BORNE A FAIRE UNE INTERPOLATION
!-- LINEAIRE DES DEPLACEMENTS DE LA MAILLE MAITRE...
!
            iinc=0
!       On teste si la liaison est incompatible
            call getvtx('LIAISON', 'GROUP_MA_MAIT_1', iocc=i, scal=k8bid, nbret=irep11)
            call getvtx('LIAISON', 'MAILLE_MAIT_1', iocc=i, scal=k8bid, nbret=irep12)
            call getvtx('LIAISON', 'GROUP_MA_MAIT_2', iocc=i, scal=k8bid, nbret=irep21)
            call getvtx('LIAISON', 'MAILLE_MAIT_2', iocc=i, scal=k8bid, nbret=irep22)
            if ((irep11.ne.0) .or. (irep12.ne.0)) then
                motcle(1) = 'MAILLE_MAIT_1'
                motcle(2) = 'GROUP_MA_MAIT_1'
                call prjlis(mod1, ma1, mod2, ma2, nbno1,&
                            nbno2, motcle, lint1, lint2, intf1,&
                            intf2, fpli1o, fpli2o, zi(ldpmli+(i-1) *9), zi(ldpmli+(i-1)*9+3),&
                            i, matprj, nomres, sst1, sst2)
                nblig=zi(ldpmli+(i-1)*9+3)
                iinc=1
            else if ((irep21.ne.0).or.(irep22.ne.0)) then
                motcle(1) = 'MAILLE_MAIT_2'
                motcle(2) = 'GROUP_MA_MAIT_2'
                call prjlis(mod2, ma2, mod1, ma1, nbno2,&
                            nbno1, motcle, lint2, lint1, intf2,&
                            intf1, fpli2o, fpli1o, zi(ldpmli+(i-1) *9+3), zi(ldpmli+(i-1)*9),&
                            i, matprj, nomres, sst2, sst1)
                nblig=zi(ldpmli+(i-1)*9)
                iinc=2
            else
                nblig=zi(ldpmli+(i-1)*9)
            endif
!
!  MATRICE LAGRANGE-LAGRANGE
!
            iad=ldpmli+(i-1)*9+6
            zi(iad)=nblig
            zi(iad+1)=2
            zi(iad+2)=nbbloc+1
            icar(1)=zi(iad)
            icar(2)=zi(iad+1)
            icar(3)=zi(iad+2)
            icar(4)=1
!
            nbbloc=nbbloc+1
!
!-------------------------DETERMINATION MATRICES ORIENTEES--------------
!
!   ROTATION DES MATRICES DE LIAISON DE LA LIAISON COURANTE
!
            if (iinc .eq. 0) then
                call verili(nomres, i, fpli1o, fpli2o, iret)
                if (iret .gt. 0) then
                    call utmess('F', 'ALGORITH12_38')
                endif
!
                iad=ldpmli+(i-1)*9
                call rotlis(nomres, fmlia, zi(iad), fpli1n, fpli1o,&
                            i, sst1, intf1, un)
                iad=ldpmli+(i-1)*9+3
                call rotlis(nomres, fmlia, zi(iad), fpli2n, fpli2o,&
                            i, sst2, intf2, moins1)
!
            else
                if (iinc .eq. 1) then
                    call inclis(nomres, sst1, sst2, intf1, intf2,&
                                fmlia, fpli1n, fpli2n, fpli1o, fpli2o,&
                                zi(ldpmli+(i-1)*9), zi(ldpmli+(i-1)*9+3), i, matprj)
                else if (iinc.eq.2) then
                    call inclis(nomres, sst2, sst1, intf2, intf1,&
                                fmlia, fpli2n, fpli1n, fpli2o, fpli1o,&
                                zi(ldpmli+(i-1)*9+3), zi(ldpmli+(i-1)*9), i, matprj)
                endif
                call jedetr(matprj)
            endif
!
!  MATRICE LAGRANGE-LAGRANGE
!
            iad=ldpmli+(i-1)*9+6
            call inilag(fmlia, icar)
!
!--------------------------------------------------C
!--                                              --C
!-- INTERFACES DEFINIES AVEC DES DDL GENERALISES --C
!--                                              --C
!---------------------------------------------------C
!
!   DANS CE CAS, ON NE PEUT PAS ECRIRE
!             L1.Q1+L2.Q2=0,
!       OU Q1 ET Q2 SONT DES DDL PHYSUQUES
!
!   ICI, ON A :  Q1=PHI1.QG1   ET  Q2=PHI2.QG2
!       PHI1 / PHI2 : BASE MODALE
!       QG1 / QG2 : DDL GENERALISES
!
!   ON ASSURE DONC LA CONTINUITE DANS LE
!   SOUS ESPACE ENGENDRE PAR PHI1 OU PAR PHI2.
!   ON CHOISI ICI LA SOUS STRUTURE ESCLAVE (PAR EX. PHI1)
!   ON CONSTRUIT DONC LA RELATION LG1.QG1+LG2.QG2=0, SOIT
!   PHI1^T(L1.PHI1).QC1 + PHI1^T(L2.PHI2).QC2=0
!
!   CETTE RELATION NE PERMET DONC PAS UN RECOLLEMNT PARFAIT
!   A L'INTERFACE, EN PARTICULIER SI PHI1 ET PHI2 SONT TRES
!   DIFFERENTES. DANS CE CAS, ON VERIFIE LE RANG DE PHI1^T(L2.PHI2)
!   ET ON FILTRE LES RELATIONS MAL PROJETEES, EN AVERTISSANT
!   L'UTILISATEUR
!
        else if (option(1:6).eq.'REDUIT') then
!
!----------------------------------------------------------------C
!--                                                            --C
!-- EXTRACTION ET ROTATION DE LA TRACE DES MODES A L'INTERFACE --C
!--                                                            --C
!----------------------------------------------------------------C
!
!-- VERIFICATION DE LA COMPATIBILITE DES NOEUDS
!
            iret=i
            call vecomo(nomres, sst1, sst2, intf1, intf2,&
                        iret, option)
!          IF (IRET .EQ.1) THEN
!
!            VERIFIER ICI QUE, DANS CE CAS, ALORS CHAQUE NOEUD
!            PORTE LES MEMES DDL QUE SONT VIS A VIS
!            SINON, IRET=0
!
!          ENDIF
!
!-- SOUS STRUCTURE 1
            lino1='&&VECT_NOEUD_INTERF1'
            indin1='&&VECT_IND_DDL_INTERF1'
            tramo1='&&MATR_TRACE_MODE_INT1  '
!
            call rotlir(nomres, sst1, intf1, lino1, 0,&
                        indin1, tramo1, ddla1, nbeq1, 1,&
                        i)
!
!-- SOUS STRUCTURE 2
            lino2='&&VECT_NOEUD_INTERF2'
            indin2='&&VECT_IND_DDL_INTERF2'
            tramo2='&&MATR_TRACE_MODE_INT2  '
!
            call rotlir(nomres, sst2, intf2, lino2, iret,&
                        indin2, tramo2, ddla2, nbeq2, 2,&
                        i)
!
!----------------------------------------C
!--                                    --C
!-- RECUPERATION DE L'INTERFACE MAITRE --C
!--                                    --C
!----------------------------------------C
!
            call getvtx('LIAISON', 'GROUP_MA_MAIT_1', iocc=i, scal=k8bid, nbret=irep11)
            call getvtx('LIAISON', 'MAILLE_MAIT_1', iocc=i, scal=k8bid, nbret=irep12)
            call getvtx('LIAISON', 'GROUP_MA_MAIT_2', iocc=i, scal=k8bid, nbret=irep21)
            call getvtx('LIAISON', 'MAILLE_MAIT_2', iocc=i, scal=k8bid, nbret=irep22)
!
            if ((irep21.ne.0) .or. (irep22.ne.0)) then
                imast=2
                if (iret .eq. 0) then
                    call lipsrb(nomres, matprj, sst1, sst2, intf1,&
                                intf2, lino1, lino2, indin1, indin2,&
                                ddla2, ddla1, nbeq2, nbeq1, imast,&
                                tramo2)
                endif
!-- ET ON PROJETTE L'EQUATION DE LIAISON SUR PHI1
!
            else
!-- SI ON NE PRECISE RIEN, C'EST LA SOUS STRUCTURE 1 QUI EST MAITRE
                imast=1
                if (iret .eq. 0) then
                    call lipsrb(nomres, matprj, sst1, sst2, intf1,&
                                intf2, lino1, lino2, indin1, indin2,&
                                ddla1, ddla2, nbeq1, nbeq2, imast,&
                                tramo1)
!-- ET ON PROJETTE L'EQUATION DE LIAISON SUR PHI2
                endif
!
            endif
!
!-------------------------------------------------------------------C
!--                                                               --C
!-- PROJECTION DES MATRICES DE LIAISON SUR LA BASE MODALE ESCLAVE --C
!--                                                               --C
!-------------------------------------------------------------------C
!
            indcol='BLANC'
            nbcol=0
!
            if (imast .lt. 2) then
!
                iad=ldpmli+(i-1)*9
                nbbloc=nbbloc+1
                zi(iad+2)=nbbloc
                call liared(nomres, fmlia, nbbloc, tramo1, ddla2,&
                            nbeq1, tramo2, ddla2, nbeq2, taille,&
                            indcol, nbcol)
                zi(iad)=taille(1)
                zi(iad+1)=taille(2)
!
                iad=ldpmli+(i-1)*9+3
                nbbloc=nbbloc+1
                zi(iad+2)=nbbloc
                call liared(nomres, fmlia, nbbloc, tramo2, ddla2,&
                            nbeq2, tramo2, ddla2, nbeq2, taille,&
                            indcol, nbcol)
                zi(iad)=taille(1)
                zi(iad+1)=taille(2)
                nblig=taille(1)
!
            else
!
                iad=ldpmli+(i-1)*9
                nbbloc=nbbloc+1
                zi(iad+2)=nbbloc
                call liared(nomres, fmlia, nbbloc, tramo1, ddla1,&
                            nbeq1, tramo1, ddla1, nbeq1, taille,&
                            indcol, nbcol)
                zi(iad)=taille(1)
                zi(iad+1)=taille(2)
!
                iad=ldpmli+(i-1)*9+3
                nbbloc=nbbloc+1
                zi(iad+2)=nbbloc
                call liared(nomres, fmlia, nbbloc, tramo2, ddla1,&
                            nbeq2, tramo1, ddla1, nbeq1, taille,&
                            indcol, nbcol)
                zi(iad)=taille(1)
                zi(iad+1)=taille(2)
!
                nblig=taille(1)
!
            endif
!
            call jedetr(indcol)
!
!  MATRICE LAGRANGE-LAGRANGE
!
            iad=ldpmli+(i-1)*9+6
            zi(iad)=nblig
            zi(iad+1)=2
            zi(iad+2)=nbbloc+1
            nbbloc=nbbloc+1
            icar(1)=zi(iad)
            icar(2)=zi(iad+1)
            icar(3)=zi(iad+2)
            icar(4)=imast
            call inilag(fmlia, icar)
!
!-- DESTRUCTION DES CONCEPTS TEMPORAIRES
!
            call jedetr(indin1)
            call jedetr(lino1)
            call jedetr(tramo1)
!
            call jedetr(indin2)
            call jedetr(lino2)
            call jedetr(tramo2)
!
        endif
!
10  end do
!
!   DESTRUCTIONS COLLECTIONS VOLATILES DE TRAVAIL
!
    call jedetr(fpli1o)
    call jedetr(fpli2o)
    call jedetr(fpli1n)
    call jedetr(fpli2n)
!
    call jedema()
end subroutine
