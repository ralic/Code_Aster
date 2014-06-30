subroutine algocu(deficu, resocu, solveu, lmat, ldscon,&
                  cncine, resu, ctccvg)
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
#include "asterc/r8maem.h"
#include "asterc/r8prem.h"
#include "asterfort/caladu.h"
#include "asterfort/calatm.h"
#include "asterfort/cuacat.h"
#include "asterfort/cuadu.h"
#include "asterfort/cudisi.h"
#include "asterfort/cuelpv.h"
#include "asterfort/cuimp1.h"
#include "asterfort/cuimp2.h"
#include "asterfort/cupivo.h"
#include "asterfort/cutabl.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/r8inir.h"
#include "asterfort/rldlgg.h"
#include "asterfort/tldlgg.h"
#include "blas/daxpy.h"
    character(len=24) :: deficu
    character(len=24) :: resocu
    character(len=19) :: solveu, cncine, resu
    integer :: lmat
    integer :: ldscon
    integer :: ctccvg
!
! ----------------------------------------------------------------------
!
! ROUTINE LIAISON_UNILATER (RESOLUTION)
!
! ALGO. DES CONTRAINTES ACTIVES POUR LES LIAISONS UNILATERALES
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICU  : SD DE DEFINITION (ISSUE D'AFFE_CHAR_MECA)
! IN  RESOCU  : SD DE TRAITEMENT NUMERIQUE
! IN  SOLVEU  : SD SOLVEUR
! IN  LMAT    : DESCRIPTEUR DE LA MATR_ASSE DU SYSTEME MECANIQUE
! IN  LDSCON  : DESCRIPTEUR DE LA MATRICE -A.C-1.AT
! VAR RESU    : INCREMENT "DDEPLA" DE DEPLACEMENT DEPUIS DEPTOT
!                 EN ENTREE : SOLUTION OBTENUE SANS TRAITER LE CONTACT
!                 EN SORTIE : SOLUTION CORRIGEE PAR LE CONTACT
! OUT CTCCVG : CODE RETOUR CONTACT DISCRET
!                -1 : PAS DE CALCUL DU CONTACT DISCRET
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : NOMBRE MAXI D'ITERATIONS
!                 2 : MATRICE SINGULIERE
!
!
!
!
    complex(kind=8) :: cbid
    logical(kind=1) :: trouac, delpos, lelpiv
    integer :: ier, ifm, niv, ndeci, isingu, npvneg
    integer :: ii, kk, iter, iliac, neqmax
    integer :: indic, kkmin, llmin
    integer :: lliac, jdecal, posnbl
    integer :: indfac, ajliai, spliai, posit, spavan
    integer :: neq, nbliac, nbliai, nbddl
    real(kind=8) :: ajeu, rho, rhorho, aadelt, rminmu, val
    real(kind=8) :: xjvmax, x1
    character(len=1) :: typeaj, typesp
    character(len=24) :: apcoef, apjeu, apddl, coco
    integer :: japcoe, japjeu, japddl, jcoco
    character(len=24) :: poinoe
    integer :: jpoi
    integer :: nnocu
    character(len=19) :: liac, mu, delt0, delta, cm1a, atmu
    integer :: jliac, jmu, jdelt0, jdelta, jcm1a, jatmu
    integer :: itemax, compts
    real(kind=8), pointer :: vale(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
! ----------------------------------------------------------------------
!
! DELTA  : INCREMENT DONNE PAR CHAQUE ITERATION DE CONTRAINTES ACTIVES.
!          C'EST D/K+1.
!
!
    call infdbg('CONTACT', ifm, niv)
!
    if (niv .ge. 2) then
        write (ifm,*) '<LIA_UNIL> <> ALGORITHME   : CONT. ACTIVES'
    endif
!
! --- LECTURE DES STRUCTURES DE DONNEES
!
    poinoe = deficu(1:16)//'.POINOE'
    apcoef = resocu(1:14)//'.APCOEF'
    apjeu = resocu(1:14)//'.APJEU'
    apddl = resocu(1:14)//'.APDDL'
    liac = resocu(1:14)//'.LIAC'
    mu = resocu(1:14)//'.MU'
    delt0 = resocu(1:14)//'.DEL0'
    delta = resocu(1:14)//'.DELT'
    cm1a = resocu(1:14)//'.CM1A'
    atmu = resocu(1:14)//'.ATMU'
    coco = resocu(1:14)//'.COCO'
! ======================================================================
    call jeveuo(poinoe, 'L', jpoi)
    call jeveuo(apcoef, 'L', japcoe)
    call jeveuo(apjeu, 'E', japjeu)
    call jeveuo(apddl, 'L', japddl)
    call jeveuo(liac, 'E', jliac)
    call jeveuo(atmu, 'E', jatmu)
    call jeveuo(mu, 'E', jmu)
    call jeveuo(delt0, 'E', jdelt0)
    call jeveuo(delta, 'E', jdelta)
    call jeveuo(coco, 'E', jcoco)
    call jeveuo(resu(1:19)//'.VALE', 'E', vr=vale)
! ======================================================================
! --- INITIALISATION DE VARIABLES
! --- NBLIAI : NOMBRE DE LIAISONS
! --- NBLIAC : NOMBRE DE LIAISONS ACTIVES
! --- NEQ    : NOMBRE D'EQUATIONS DU MODELE
! --- ITEMAX : NOMBRE D'ITERATIONS DANS L'ALGO
! --- INDFAC : INDICE DE DEBUT DE LA FACTORISATION
! --- INDIC  : 0  INITIALISATION,
!             +1 ON A RAJOUTE UNE LIAISON
!             -1 ON A ENLEVE UNE LIAISON
! --- SPLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
!              LIAISON AYANT ETE CALCULEE POUR LE VECTEUR CM1A
! --- AJLIAI : INDICE DANS LuA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
!              LIAISON CORRECTE DU CALCUL
!              DE LA MATRICE  ACM1AT
! ======================================================================
    nnocu = cudisi(deficu,'NNOCU')
    nbliai = nnocu
    neq = zi(lmat+2)
    itemax = 2*nbliai
    typeaj = 'A'
    typesp = 'S'
    indic = 0
    indfac = 1
    ajliai = 0
    spliai = 0
    nbliac = 0
    xjvmax = 0.0d0
    iter = 0
! ======================================================================
!                             INITIALISATIONS
!
!
! --- RECOPIE DANS DELT0 DU CHAMP DE DEPLACEMENTS OBTENU SANS
! --- TRAITER LES CONDITIONS UNILATERALES
! --- CREATION DE DELTA0 = C-1B
!
    do 10 kk = 1, neq
        zr(jdelt0-1+kk) = vale(kk)
        vale(kk) = 0.0d0
10  end do
! ======================================================================
! --- DETECTION DES COUPLES DE NOEUDS ACTIVES
! --- ON CALCULE LE NOUVEAU JEU : AJEU+ = AJEU/I/N - A.DDEPLA
! --- (IL EST NEGATIF LORSQU'IL Y A ACTIVATION -> LIAISON ACTIVE)
! ======================================================================
    if (niv .eq. 2) then
        write(ifm,*)'<LIA_UNIL> <> LIAISONS INITIALES '
    endif
    if (nbliac .eq. 0) then
        do 30 ii = 1, nbliai
            jdecal = zi(jpoi+ii-1)
            nbddl = zi(jpoi+ii) - zi(jpoi+ii-1)
            call caladu(neq, nbddl, zr(japcoe+jdecal), zi(japddl+jdecal), zr(jdelt0),&
                        val)
            ajeu = zr(japjeu+ii-1) - val
!
            if (ajeu .lt. 0.0d0) then
                indic = 0
                posit = nbliac + 1
                call cutabl(indic, nbliac, ajliai, spliai, resocu,&
                            typeaj, posit, ii)
                if (niv .ge. 2) then
                    call cuimp2(ifm, ii, typeaj, 'ALG', resocu)
                endif
            endif
!
30      continue
    endif
!
    if (niv .ge. 2) then
        write(ifm,1000) nbliai
        write(ifm,1005) nbliac
        write(ifm,1001) itemax
    endif
!
! ======================================================================
!                    REPRISE DE LA BOUCLE PRINCIPALE
! ======================================================================
!
40  continue
!
! ======================================================================
! ---
! --- RESOLUTION MATRICIELLE POUR DES LIAISONS ACTIVES
! ---
! ======================================================================
!
!
! ======================================================================
! --- SI PAS DE LIAISON ACTIVE, ON REMPLIT DELTA ET ON VA
! --- DIRECTEMENT AU CALCUL DE RHO
! ======================================================================
!
    if (nbliac .eq. 0) then
        do 50 kk = 1, neq
            zr(jdelta+kk-1) = zr(jdelt0+kk-1) - vale(kk)
50      continue
    endif
!
! ======================================================================
! --- S'IL Y A DES LIAISONS ACTIVES, ON CALCULE MU ET DELTA
! ======================================================================
!
!
! --- DETERMINATION DE LA 1ERE LIAISON AYANT CHANGE D'ETAT (IN/ACTIF)
! --- (ON NE RECONSTRUIRA -A.C-1.AT QU'A PARTIR DE CETTE LIAISON)
!
    if (nbliac .ne. 0) then
!
!
! --- PAR LDLT OU MULT_FRONT
!
        spavan = spliai
!
! --- CALCUL DE -A.C-1.AT COLONNE PAR COLONNE (A PARTIR DE INDFAC)
!
        call cuacat(indic, nbliac, ajliai, spliai, lmat,&
                    indfac, deficu, resocu, solveu, cncine,&
                    xjvmax)
!
! --- ELIMINATION DES PIVOTS NULS
!
        call cupivo(xjvmax, indic, nbliac, ajliai, spliai,&
                    spavan, deficu, resocu)
!
! --- ON A SUPPRIME UNE LIAISON
!
        if (indic .eq. -1) then
            goto 150
        endif
!
!
! --- FACTORISATION LDLT DE -A.C-1.AT
!
        if (indfac .le. nbliac) then
            if (niv .ge. 2) then
                write(ifm,*)'<LIA_UNIL> <> FACTORISATION MATRICE'
            endif
!
            call tldlgg(2, ldscon, indfac, nbliac, 0,&
                        ndeci, isingu, npvneg, ier)
!
            indfac = nbliac + 1
!
! --- LA MATRICE DE CONTACT EST-ELLE SINGULIERE ?
!
            if (ier .gt. 0) then
                ctccvg = 2
                goto 999
            endif
        endif
!
! --- SECOND MEMBRE : ON MET JEU(DEPTOT) - A.DELT0 DANS MU
!
        call cuadu(deficu, resocu, neq, nbliac)
!
! --- RESOLUTION POUR OBTENIR MU : -A.C-1.AT.MU = JEU(DEPTOT) - A.DELT0
! --- ON TRUANDE LA SD MATR_ASSE POUR NE RESOUDRE LE SYSTEME QUE
! --- DE 1 A NBLIAC
!
        neqmax = zi(ldscon+2)
        zi(ldscon+2) = nbliac
        call rldlgg(ldscon, zr(jmu), [cbid], 1)
        zi(ldscon+2) = neqmax
!
! --- CALCUL DE DELTA = DELT0 - C-1.AT.MU
!
        do 70 kk = 1, neq
            zr(jdelta-1+kk) = zr(jdelt0-1+kk) - vale(kk)
70      continue
!
! --- MISE A JOUR DU VECTEUR DEPLACEMENT <DU> CORRIGE
!
        posnbl = 0
        do 71 iliac = 1, nbliac
            lliac = zi(jliac-1+iliac)
            posnbl = posnbl + 1
            call jeveuo(jexnum(cm1a, lliac), 'L', jcm1a)
            call daxpy(neq, -zr(jmu-1+posnbl), zr(jcm1a), 1, zr(jdelta),&
                       1)
            call jelibe(jexnum(cm1a, lliac))
71      continue
!
    endif
!
!
!
! --- CALCUL DE RHO = MIN ( (E(DEPTOT) - A.RESU)II / (A.DELTA)II) )
! --- SUR LES LIAISONS NON ACTIVES DE NUMERO II
!
    rho = r8maem()
    delpos = .false.
!
    if (nbliac .eq. nbliai) then
! ======================================================================
! -- SI TOUTES LES LIAISONS SONT ACTIVES : RHO = 1
! ======================================================================
        rho = 1.d0
    else if (nbliac.lt.nbliai) then
! ======================================================================
! -- S'IL Y A DES LIAISONS NON ACTIVES : CALCUL DE RHO
! ======================================================================
        do 112 ii = 1, nbliai
            trouac = .false.
! ======================================================================
! -- LA LIAISON II EST-ELLE ACTIVE ? (-> TROUAC)
! ======================================================================
            do 90 iliac = 1, nbliac
                if (zi(jliac-1+iliac) .eq. ii) trouac = .true.
90          continue
! ======================================================================
! -- CALCUL DE A.DELTA SI LA LIAISON II N'EST PAS ACTIVE
! ======================================================================
            if (.not.trouac) then
                jdecal = zi(jpoi+ii-1)
                nbddl = zi(jpoi+ii) - zi(jpoi+ii-1)
                call caladu(neq, nbddl, zr(japcoe+jdecal), zi(japddl+ jdecal), zr(jdelta),&
                            aadelt)
!
! ======================================================================
! -- SI A.DELTA EST POSITIF POUR II : CALCUL DE E(DEPTOT) - A.RESU
! -- RHO = MIN ( ( E(DEPTOT) - A.RESU )II / (A.DELTA)II )
! -- ON STOCKE DANS LLMIN LE NUMERO DE LA LIAISON REALISANT LE
! -- MINIMUM (CE SERA LA LIAISON LA PLUS VIOLEE)
! ======================================================================
                if (aadelt .gt. r8prem()) then
! ======================================================================
! -- ON NE PREND PAS EN COMPTE UNE LIAISON A PIVOT NUL
! ======================================================================
                    call cuelpv(ii, resocu, nbliai, lelpiv)
                    if (lelpiv) then
                        goto 112
                    endif
                    delpos = .true.
                    call caladu(neq, nbddl, zr(japcoe+jdecal), zi(japddl+ jdecal), vale,&
                                val)
!
                    ajeu = zr(japjeu+ii-1) - val
                    ajeu = ajeu/aadelt
                    if (ajeu .lt. rho) then
                        rho = ajeu
                        llmin = ii
                    endif
                endif
            endif
112      continue
! ======================================================================
! -- SI TOUS LES (A.DELTA)II SONT NEGATIFS : RHO = 1
! ======================================================================
        if (.not.delpos) then
            rho = 1.0d0
        endif
    endif
!
! --- TESTS SUR RHO ET ACTUALISATION DE RESU
!
    x1 = 1.d0
    rhorho = min(rho,x1)
!
    do 120 kk = 1, neq
        vale(kk) = vale(kk) + rhorho*zr(jdelta-1+kk)
120  end do
!
! -- SI RHO < 1 (AU MOINS UNE LIAISON SUPPOSEE NON ACTIVE EST VIOLEE) :
! -- ON AJOUTE A L'ENSEMBLE DES LIAISONS ACTIVES LA PLUS VIOLEE (LLMIN)
!
    if (rho .lt. 1.0d0) then
        posit = nbliac + 1
        call cutabl(indic, nbliac, ajliai, spliai, resocu,&
                    typeaj, posit, llmin)
        if (niv .ge. 2) then
            call cuimp2(ifm, llmin, typeaj, 'ALG', resocu)
        endif
    else
!
! -- SI RHO > 1 OU RHO = 1
!
! - SI PAS DE LIAISONS ACTIVES -> ON A CONVERGE
!
        if (nbliac .eq. 0) then
            goto 160
        endif
!
        rminmu = r8maem()
        do 130 iliac = 1, nbliac
            if (rminmu .gt. zr(jmu-1+iliac)) then
                rminmu = zr(jmu-1+iliac)
                kkmin = iliac
            endif
130      continue
!
!
! - SI TOUS LES MU SONT > 0 -> ON A CONVERGE
!
        if (rminmu .ge. 0.0d0) then
            goto 160
        endif
! ======================================================================
! - SINON ON ENLEVE LA LIAISON KKMIN AYANT LE MU LE PLUS NEGATIF
! - ET ON DECALE LA LISTE DES LIAISONS ACTIVES
! - ATTENTION KKMIN EST UN INDICE DANS LA LISTE DES LIAISONS <ACTIVES>
! - ET NON DANS LA LISTE DE TOUTES LES LIAISONS POSSIBLES
! ======================================================================
        lliac = zi(jliac-1+kkmin)
        call cutabl(indic, nbliac, ajliai, spliai, resocu,&
                    typesp, kkmin, lliac)
!
        if (niv .ge. 2) then
            call cuimp2(ifm, lliac, typesp, 'ALG', resocu)
        endif
!
    endif
! ======================================================================
! - ON PASSE A L'ITERATION DE CONTRAINTES ACTIVES SUIVANTES
! ======================================================================
150  continue
    iter = iter + 1
!
!
! --- A-T-ON DEPASSE LE NOMBRE D'ITERATIONS
!
    if (iter .gt. itemax+1) then
        ctccvg = 1
        goto 999
    endif
!
    goto 40
!
! ======================================================================
!                            ON A CONVERGE
! ======================================================================
!
160  continue
!
!
! --- CALCUL DES FORCES (AT.MU)
!
    call r8inir(neq, 0.d0, zr(jatmu), 1)
!
! --- CODE RETOUR
!
    ctccvg = 0
!
    compts = 0
    do 161 iliac = 1, nbliac
        lliac = zi(jliac+iliac-1)
        jdecal = zi(jpoi+lliac-1)
        nbddl = zi(jpoi+lliac) - zi(jpoi+lliac-1)
        compts = compts + 1
        call calatm(neq, nbddl, zr(jmu-1+compts), zr(japcoe+jdecal), zi( japddl+jdecal),&
                    zr(jatmu))
161  end do
!
!
! --- MAJ DU JEU (IL N'EST RECALCULE QU'EN DEBUT DE PAS DE TPS)
!
!
    do 162 iliac = 1, nbliai
        jdecal = zi(jpoi+iliac-1)
        nbddl = zi(jpoi+iliac) - zi(jpoi+iliac-1)
        call caladu(neq, nbddl, zr(japcoe+jdecal), zi(japddl+jdecal), vale,&
                    val)
        zr(japjeu+iliac-1) = zr(japjeu+iliac-1) - val
162  end do
!
    zi(jcoco+2) = nbliac
!
! --- AFFICHAGE FINAL
!
    if (niv .ge. 2) then
        write(ifm,1002) iter
        write(ifm,1003) nbliac
        call cuimp1(deficu, resocu, ifm)
    endif
!
999  continue
! ======================================================================
! --- DESTRUCTION DES VECTEURS INUTILES
! ======================================================================
!
    call jedema()
!
    1000 format (' <LIA_UNIL> <> NOMBRE DE LIAISONS POSSIBLES: ',i6)
    1001 format (' <LIA_UNIL> <> DEBUT DES ITERATIONS (MAX: ',i6,')')
    1002 format (' <LIA_UNIL> <> FIN DES ITERATIONS (NBR: ',i6,')')
    1003 format (' <LIA_UNIL> <> NOMBRE DE LIAISONS FINALES:',&
     &       i6,')')
    1005 format (' <LIA_UNIL> <> NOMBRE DE LIAISONS INITIALES:',&
     &       i6,')')
! ======================================================================
!
end subroutine
