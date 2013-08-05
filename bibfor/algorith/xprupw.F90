subroutine xprupw(cmnd, noma, fispre, vcn, grlr,&
                  noesom, lcmin, cnsln, grln, cnslt,&
                  grlt, deltat, noresi, isozro, nodtor,&
                  eletor, liggrd)
    implicit none
!
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/celces.h"
#include "asterfort/cescns.h"
#include "asterfort/cnscno.h"
#include "asterfort/dismoi.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeundf.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
#include "asterfort/xprls0.h"
#include "asterfort/xprpfi.h"
    character(len=8) :: cmnd, noma, fispre
    character(len=19) :: cnsln, grln, cnslt, grlt, noresi, noesom, isozro
    character(len=19) :: nodtor, eletor, liggrd
    character(len=24) :: vcn, grlr
    real(kind=8) :: deltat, lcmin
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
! person_in_charge: samuel.geniaut at edf.fr
!
!     ------------------------------------------------------------------
!
!       XPRUPW   : X-FEM PROPAGATION : REINITIALISATION ET
!                                      REORTHOGONALISATION DES LEVEL
!                                      SETS AVEC LA METHODE UPWIND
!       ------     -     --                                 ---
!
!  DANS LE CADRE DE LA PROPAGATION X-FEM, UTILISATION DE LA METHODE
!  UPWIND POUR LES PHASES DE REINITIALISATION ET REORTHOGONALISATION
!  DES LEVEL SETS APRES LA MISE A JOUR
!
!
!    ENTREE
!    ------
!      CMND   = 'REINITLN' POUR LA REINITIALISATION DE LA LEVEL SET
!                          NORMALE
!               'REINITLT' POUR LA REINITIALISATION DE LA LEVEL SET
!                          TANGENTE
!               'REORTHOG' POUR LA REORTHOGONALISATION DE LA LEVEL SET
!                          TANGENTE PAR RAPPORT A LA LEVEL SET NORMALE
!      NOMA   = NOM DU MAILLAGE DU MODELE
!      FISPRE = NOM DU CONCEPT FISSURE X-FEM DE LA FISSURE ACTUELLE
!      VCN    = VOIR XPRCNU.F POUR LA DESCRIPTION DE CETTE OBJET.
!      GRLR   = VOIR XPRCNU.F POUR LA DESCRIPTION DE CETTE OBJET.
!      NOESOM = VECTEUR LOGIQUE CONTENANT L'INFO 'NOEUD SOMMET'
!      LCMIN  = LONGEUR DE PLUS PETIT ARETE DU MAILLAGE NOMA
!      CNSLN  = CHAMP_NO_S DES VALEURS DE LA LEVEL SET NORMALE
!      GRLN   = CHAMP_NO_S DES VALEURS DU GRADIENT DE CNSLN
!      CNSLT  = CHAMP_NO_S DES VALEURS DE LA LEVEL SET TANGENTE
!      GRLT   = CHAMP_NO_S DES VALEURS DU GRADIENT DE CNSLT
!      DELTAT = PAS DU TEMPS VIRTUEL A UTILISER POUR L'INTEGRATIONS DES
!               EQUATIONS DIFFERENTIELLES
!      NORESI = VECTEUR LOGIQUE INDIQUANT SI LE RESIDU DOIT ETRE ESTIME
!               SUR LE NOEUD
!      ISOZRO = VECTEUR LOGIQUE INDIQUANT SI LA "VRAIE" LEVEL SET
!               (DISTANCE SIGNEE) A ETE CALCULEE POUR LE NOEUD
!      NODTOR = LISTE DES NOEUDS DEFINISSANTS LE DOMAINE DE CALCUL
!      ELETOR = LISTE DES ELEMENTS DEFINISSANTS LE DOMAINE DE CALCUL
!      LIGGRD = LIGREL DU DOMAINE DE CALCUL (VOIR XPRTOR.F)
!
!    SORTIE
!    ------
!      CNSLN  = CHAMP_NO_S DES NOUVELLES VALEURS DE LA LEVEL SET NORMALE
!      GRLN   = CHAMP_NO_S DES NOUVELLES VALEURS DU GRADIENT DE CNSLN
!      CNSLT  = CHAMP_NO_S DES NOUVELLES VALEURS DE LA LEVEL SET
!               TANGENTE
!      GRLT   = CHAMP_NO_S DES NOUVELLES VALEURS DU GRADIENT DE CNSLT
!
!     ------------------------------------------------------------------
!
!
    character(len=19) :: cnsls, grls, lsv, grlsv
    character(len=2) :: levset
    integer :: jzero
    logical :: reinit
!
!     MESH INFORMATION RETREIVING AND GENERAL PURPOSE VARIABLES
    integer :: nbno, nbnoma, jcoor, jcnsls, jgrls
    integer :: jlsv, jgrlsv, node, nodeps, ndim
    integer :: ifm, niv, iret, jnores, jnodto, jelcal, neleto
    character(len=8) :: k8b
    integer :: i, j, k
!
!     CONNECTION TABLE OF THE NODES
    integer :: jvcn, jvcnd, jref, jgrlr
    real(kind=8) :: ref(3, 3)
!
!     MINIMIZATION LOOP
    character(len=24) :: tempv
    integer :: jtempv
    real(kind=8) :: sgnls, vtmp, vxyz(3), vxyzgl(3), modgrl
    integer :: itrmax
    parameter        (itrmax=300)
!
!     EVALUATION OF THE GRADIENT OF THE LEVEL SET
    character(len=8) :: lpain(4), lpaout(2)
    character(len=19) :: cnols, celgls, chams
    character(len=24) :: lchin(4), lchout(2)
    integer :: ibid
!
!     EVALUATION OF THE RESIDUAL
    real(kind=8) :: resglo(itrmax), restor, dlsg, dlsl, sumlsg, sumlsl
    real(kind=8) :: prevls
    real(kind=8) :: resiln, resilt, resort, resils, resigl
    parameter        (resiln = 1.d-7)
    parameter        (resilt = 1.d-7)
    parameter        (resort = 1.d-7)
    parameter        (resigl = 1.d-9)
!
!     UPWIND PROBLEMATIC POINTS
    character(len=19) :: poifis, trifis, forced
    integer :: jforce
    real(kind=8) :: p(3), lvsp, lsnpc, lstpc
    logical :: grad0
!
!-----------------------------------------------------------------------
!     DEBUT
!-----------------------------------------------------------------------
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)
!
!     JEVEUX OBJECTS WHERE THE POINTS FORMING THE LSN=0 WILL BE STORED
    poifis = '&&XPRUPW.POIFIS'
    trifis = '&&XPRUPW.TRIFIS'
!
!     RETRIEVE THE DIMENSION OF THE PROBLEM
    call dismoi('F', 'DIM_GEOM', noma, 'MAILLAGE', ndim,&
                k8b, iret)
!
!     DECODE THE COMMAND TO BE EXECUTED AND PREPARE THE CORRECT INPUT
!     FIELDS FOR THE LEVEL SETS AND THEIR GRADIENTS
    if (cmnd(1:6) .eq. 'REINIT') then
!
        reinit = .true.
        levset = cmnd(7:8)
!        SET THE WORKING LEVEL SET FOR THE REINITIALIZATION PHASE
        if (levset .eq. 'LN') then
            cnsls = cnsln
            grls = grln
            resils = resiln
        else
            cnsls = cnslt
            grls = grlt
            resils = resilt
        endif
!
!        RETRIEVE THE LEVEL SET AND ITS GRADIENT FOR THE INTEGRATION AT
!        t=0
        call jeveuo(cnsls//'.CNSV', 'E', jcnsls)
        call jeveuo(grls//'.CNSV', 'E', jgrls)
!
!        IN THE REINITIALIZATION PHASE, ONLY ONE LEVEL SET IS INVOLVED
!        INTO THE DIFFERENTIAL EQUATION
        lsv = cnsls
        grlsv = grls
        jlsv = jcnsls
        jgrlsv = jgrls
!
        if (niv .ge. 0) then
            write(ifm,*)'   REINITIALISATION DE LA LEVEL SET ',levset,&
     &                  ' PAR LA METHODE UPWIND'
        endif
!
    else
!
        reinit = .false.
        levset = '  '
        cnsls = cnslt
        grls = grlt
        lsv = cnsln
        grlsv = grln
        resils = resort
!
!        RETRIEVE THE LEVEL SET AND ITS GRADIENT FOR THE INTEGRATION AT
!        t=0
        call jeveuo(cnsls//'.CNSV', 'E', jcnsls)
        call jeveuo(grls//'.CNSV', 'E', jgrls)
        call jeveuo(lsv//'.CNSV', 'E', jlsv)
        call jeveuo(grlsv//'.CNSV', 'E', jgrlsv)
!
        if (niv .ge. 0) then
            write(ifm,*)'   REORTHOGONALISATION DES LEVEL SETS PAR LA '//&
     &                 'METHODE UPWIND'
        endif
!
    endif
!
!     RETRIEVE THE NUMBER OF NODES AND ELEMENTS IN THE MESH
    call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nbnoma,&
                k8b, iret)
!     RETRIEVE THE COORDINATES OF THE NODES
!                12345678901234567890
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
!
!     RETRIEVE THE NODES IN WHICH THE LOCAL RESIDUAL MUST BE CALCULATED
    call jeveuo(noresi, 'L', jnores)
!
!     RETRIEVE THE NUMBER OF THE NODES THAT MUST TO BE USED IN THE
!     CALCULUS (SAME ORDER THAN THE ONE USED IN THE CONNECTION TABLE)
    call jeveuo(nodtor, 'L', jnodto)
!
!     RETRIEVE THE TOTAL NUMBER OF THE NODES THAT MUST BE ELABORATED
    call jelira(nodtor, 'LONMAX', nbno, k8b)
!
!     RETRIEVE THE LIST OF THE ELEMENTS DEFINING THE TORE
    call jeveuo(eletor, 'L', jelcal)
!
!     RETRIEVE THE NUMBER OF ELEMENTS DEFINING THE TORE
    call jelira(eletor, 'LONMAX', neleto, k8b)
!
!     CHECK IF THE LOCALISATION OF THE DOMAIN HAS BEEN REQUESTED
    if (nbnoma .eq. nbno) then
        grad0 = .true.
    else
        grad0 = .false.
    endif
!
!     RETRIEVE THE LOCAL REFERENCE SYSTEM TO BE USED WITH THE LEVELSET
!     MESH
    call jeveuo(grlr, 'L', jgrlr)
    jref = jgrlr+1
    do 100 i = 1, 3
        ref(i,1) = zr(jref-1+3*(i-1)+1)
        ref(i,2) = zr(jref-1+3*(i-1)+2)
        ref(i,3) = zr(jref-1+3*(i-1)+3)
100  end do
!
!     DECLARE SOME DATA STRUCTURES FOR THE EVALUATION OF THE GRADIENT
    cnols = '&&XPRUPW.CNOLS'
    celgls = '&&XPRUPW.CELGLS'
    chams = '&&XPRUPW.CHAMS'
!
!----------------------------------------------------------------------
!   CALCUL DES VRAIES DISTANCES SIGNEES SUR LES NOEUDS PROCHES DE LS=0
!----------------------------------------------------------------------
!
!     THIS IS DONE ONLY FOR THE REINITIALIZATION PHASE. FOR THE
!     REORTHOG. PHASE THE DISTANCES CALCULATED IN THE PREVIOUS PHASE ARE
!     RETRIEVED
    if (reinit) then
!        VECTEUR IDIQUANT SI LS AU NOEUD EST CALCULEE
        call wkvect(isozro, 'V V L', nbnoma, jzero)
        if (levset .eq. 'LT') then
            call jedetr(poifis)
            call jedetr(trifis)
        endif
        call xprls0(fispre, noma, noesom, lcmin, cnsln,&
                    cnslt, isozro, levset, nodtor, eletor,&
                    poifis, trifis)
    else
        call jeveuo(isozro, 'L', jzero)
    endif
!
!-----------------------------------------------------------------------
! INTEGRATION OF THE DIFFERENTIAL EQUATION USING THE UPWIND METHOD
!-----------------------------------------------------------------------
!
!     RETRIEVE THE CONNECTION INFORMATION FOR THE NODES OF THE AUXILIARY
!     GRID
    call jeveuo(vcn, 'L', jvcn)
    jvcnd = jgrlr+10
!
!     PRINT INFORMATIONS ABOUT RESIDUALS AT EACH ITERATION
    if (niv .ge. 1) then
        write(ifm,910)
        write(ifm,911)
        write(ifm,912)
        write(ifm,913)
    endif
!
!     CREATE A TEMPORARY VECTOR
    tempv = '&&XPRUPW.TEMPV'
    call wkvect(tempv, 'V V R', nbno, jtempv)
!
!     CREATE A TEMPORARY FLAG VECTOR IN ORDER TO MARK THE PROBLEMATIC
!     NODES
    forced= '&&XPRUPW.FORCED'
    call wkvect(forced, 'V V L', nbno, jforce)
    call jeundf(forced)
    call jeveuo(forced, 'E', jforce)
!
!     MINIMIZATION LOOP
!     THIS LOOP IS RUN ITRMAX TIMES. IF THE CONVERGENCE IS ACHIEVED
!     BEFORE THIS NUMBER OF REPETITIONS, THE LOOP IS BROKEN ANYWAY.
    do 1000 i = 1, itrmax
!
        resglo(i) = 0
        restor = 0
!
!        LOOP ON EACH NODE OF THE MESH
        do 1100 k = 1, nbno
!
!           RETREIVE THE NODE NUMBER
            node = zi(jnodto-1+k)
!
!           SIGN(LS)
            sgnls = 0.d0
            if (abs(zr(jlsv-1+node)) .gt. r8prem()) sgnls = zr(jlsv-1+ node)/abs(zr(jlsv-1+node))
!
!
            if (ndim .eq. 2) then
!
!              NORM OF THE GRADIENT OF THE LEVELSET IN THE NODE
                modgrl = (zr( jgrlsv-1+2*(node-1)+1)**2.d0 + zr(jgrlsv- 1+2*(node-1)+2 )**2.d0&
                         )**.5d0
!
!              SIGN(LS)/NORM OF THE GRADIENT IN THE NODE
                if (modgrl .gt. r8prem()) then
                    vtmp = sgnls / modgrl
                else
                    vtmp = 0.d0
                endif
!
!              EVALUATION OF VX AND VY. VZ IS SET TO ZERO. THESE ARE
!              CALCULATED IN THE GLOBAL REFERENCE SYSTEM.
                vxyzgl(1) = vtmp * zr(jgrlsv-1+2*(node-1)+1)
                vxyzgl(2) = vtmp * zr(jgrlsv-1+2*(node-1)+2)
                vxyzgl(3) = 0
!
            else
!
!              NORM OF THE GRADIENT OF THE LEVELSET IN THE NODE
                modgrl = (&
                         zr(&
                         jgrlsv-1+3*(node-1)+1)**2.d0 + zr(jgrlsv- 1+3*(node-1)+2)**2.d0 + zr(jgr&
                         &lsv-1+3*(node-1)+3&
                         )** 2.d0&
                         )**.5d0
!
!              SIGN(LS)/NORM OF THE GRADIENT IN THE NODE
                if (modgrl .gt. r8prem()) then
                    vtmp = sgnls / modgrl
                else
                    vtmp = 0.d0
                endif
!
!              EVALUATION OF VX, VY AND VZ. THESE ARE CALCULATED
!              IN THE GLOBAL REFERENCE SYSTEM.
                vxyzgl(1) = vtmp * zr(jgrlsv-1+3*(node-1)+1)
                vxyzgl(2) = vtmp * zr(jgrlsv-1+3*(node-1)+2)
                vxyzgl(3) = vtmp * zr(jgrlsv-1+3*(node-1)+3)
!
            endif
!
!           EVALUATION OF VX, VY AND VZ IN THE LOCAL REFERENCE SYSTEM
            vxyz(1) = vxyzgl(1)*ref(1,1)+vxyzgl(2)*ref(1,2)+ vxyzgl(3) *ref(1,3)
            vxyz(2) = vxyzgl(1)*ref(2,1)+vxyzgl(2)*ref(2,2)+ vxyzgl(3) *ref(2,3)
            vxyz(3) = vxyzgl(1)*ref(3,1)+vxyzgl(2)*ref(3,2)+ vxyzgl(3) *ref(3,3)
!
!           EVALUATION OF THE DIRECTIONAL DERIVATIVES OF THE LEVEL SET
!           (ALONG X, Y AND Z)
            vtmp = 0
            do 1150 j = 1, ndim
!
                if (vxyz(j) .ge. 0) then
!
!                  NODE POSITION OF THE NEIGHBORING NODE IN THE
!                  CONNECTION TABLE
                    nodeps = 6*(k-1)+2*(j-1)+2
!
                    if (zi(jvcn-1+nodeps) .gt. 0) then
!
!                    THERE IS ONE NEIGHBORING NODE AND THEREFORE THE
!                    GRADIENT CAN BE CALCULATED
                        vtmp = vtmp + vxyz(j)* (zr(jcnsls-1+node)-zr( jcnsls-1+zi(jvcn-1+nodeps))&
                               &)/ zr(jvcnd-1+ nodeps)
!
                    else
!
!                    NO NEIGHBORING NODES! THE ESTIMATION OF THE FINAL
!                    VALUE OF THE LEVELSET IS IMPOSED IF THE DOMAIN
!                    LOCALISATION HAS BEEN REQUESTED
                        if ((.not.grad0) .and. (.not.zl(jzero-1+node)) .and.&
                            (.not.zl(jforce-1+k))) then
!                       THE VALUE OF THE LEVEL SET WILL BE FORCED FOR
!                       THIS NODE AND NOT UPDATED USING UPWIND
                            zl(jforce-1+k) = .true.
!                       RETREIVE THE COORDINATES OF THE PROBLEMATIC NODE
                            p(1) = zr(jcoor-1+3*(node-1)+1)
                            p(2) = zr(jcoor-1+3*(node-1)+2)
                            p(3) = zr(jcoor-1+3*(node-1)+3)
!                       RETREIVE THE VALUE OF THE LEVEL SET THAT IS
!                       BEING UPDATED
                            lvsp = zr(jcnsls-1+node)
!                       CALCULATE THE LEVEL SET (USING THE SIGNED
!                       DISTANCE PROPERTY)
                            call xprpfi(p, lvsp, lcmin, poifis, trifis,&
                                        fispre, ndim, lsnpc, lstpc)
                            if (reinit) then
!                          IN THIS CASE THE NORMAL LEVEL SET IS BEING
!                          UPDATED
                                zr(jcnsls-1+node) = lsnpc
                            else
!                          IN THIS CASE THE TANGENTIAL ONE
                                zr(jcnsls-1+node) = lstpc
                            endif
                        endif
                    endif
!
                else
!
!                  NODE POSITION OF THE NEIGHBORING NODE IN THE
!                  CONNECTION TABLE
                    nodeps = 6*(k-1)+2*(j-1)+1
!
                    if (zi(jvcn-1+nodeps) .gt. 0) then
!
!                    THERE IS ONE NEIGHBORING NODE AND THEREFORE THE
!                    GRADIENT CAN BE CALCULATED
                        vtmp = vtmp + vxyz(j)* (zr(jcnsls-1+zi(jvcn-1+ nodeps))-zr(jcnsls-1+node)&
                               &)/ zr(jvcnd-1+ nodeps)
!
                    else
!
!                    NO NEIGHBORING NODES! THE ESTIMATION OF THE FINAL
!                    VALUE OF THE LEVELSET IS IMPOSED IF THE DOMAIN
!                    LOCALISATION HAS BEEN REQUESTED
                        if ((.not.grad0) .and. (.not.zl(jzero-1+node)) .and.&
                            (.not.zl(jforce-1+k))) then
!                       THE VALUE OF THE LEVEL SET WILL BE FORCED FOR
!                       THIS NODE AND NOT UPDATED USING UPWIND
                            zl(jforce-1+k) = .true.
!                       RETREIVE THE COORDINATES OF THE PROBLEMATIC NODE
                            p(1) = zr(jcoor-1+3*(node-1)+1)
                            p(2) = zr(jcoor-1+3*(node-1)+2)
                            p(3) = zr(jcoor-1+3*(node-1)+3)
!                       RETREIVE THE VALUE OF THE LEVEL SET THAT IS
!                       BEING UPDATED
                            lvsp = zr(jcnsls-1+node)
!                       CALCULATE THE LEVEL SET (USING THE SIGNED
!                       DISTANCE PROPERTY)
                            call xprpfi(p, lvsp, lcmin, poifis, trifis,&
                                        fispre, ndim, lsnpc, lstpc)
                            if (reinit) then
!                          IN THIS CASE THE NORMAL LEVEL SET IS BEING
!                          UPDATED
                                zr(jcnsls-1+node) = lsnpc
                            else
!                          IN THIS CASE THE TANGENTIAL ONE
                                zr(jcnsls-1+node) = lstpc
                            endif
                        endif
                    endif
!
                endif
!
1150          continue
!
            if (reinit) then
!              SUBTRACT f(x) IN THE REINITIALIZATION CASE.
                zr(jtempv-1+k) = vtmp - sgnls
            else
!              FOR THE REORTHOGONALIZATION CASE, f(x)=0
                zr(jtempv-1+k) = vtmp
            endif
!
1100      continue
!
!        LAST STEP: EVALUATE THE NEW VALUE OF THE LEVEL SET AT EACH
!        NODE. EVALUATE ALSO THE LOCAL AND GLOBAL RESIDUALS
        sumlsg = 0
        sumlsl = 0
        dlsg = 0
        dlsl = 0
        do 1050 k = 1, nbno
!
!           RETREIVE THE NODE NUMBER
            node = zi(jnodto-1+k)
!
!           IF THE LEVEL SET HAS BEEN UPDATED PREVIOUSLY (ACROSS THE
!           CRACK FRONT OR CALCULATING THE SIGNED DISTANCE), IT IS NOT
!           CALCULATED HERE AGAIN USING UPWIND
            if ((.not.zl(jzero-1+node)) .and. (.not.zl(jforce-1+k))) then
!
                prevls = zr(jcnsls-1+node)
                zr(jcnsls-1+node) = prevls - deltat*zr(jtempv-1+k)
!
!             RESIDUALS ESTIMATION
                sumlsg = sumlsg + prevls**2
                dlsg = dlsg + (zr(jcnsls-1+node)-prevls)**2
                if (zl(jnores-1+node)) then
                    sumlsl = sumlsl + prevls**2
                    dlsl = dlsl + (zr(jcnsls-1+node)-prevls)**2
                endif
!
            endif
!
1050      continue
!
!        EVALUATION OF THE GRADIENT OF THE NEW LEVEL SET
        call cnscno(cnsls, ' ', 'NON', 'V', cnols,&
                    'F', ibid)
        lpain(1)='PGEOMER'
        lchin(1)=noma//'.COORDO'
        lpain(2)='PNEUTER'
        lchin(2)=cnols
        lpaout(1)='PGNEUTR'
        lchout(1)=celgls
!
        call calcul('S', 'GRAD_NEUT_R', liggrd, 2, lchin,&
                    lpain, 1, lchout, lpaout, 'V',&
                    'OUI')
!
        call celces(celgls, 'V', chams)
        call cescns(chams, ' ', 'V', grls, ' ',&
                    ibid)
        call jeveuo(grls//'.CNSV', 'E', jgrls)
!
        if (reinit) jgrlsv = jgrls
!
!        FINAL EVALUATION OF THE RESIDUALS.
!        GLOBAL RESIDUAL FIRST...
        ASSERT(sumlsg.gt.r8prem())
        resglo(i) = (dlsg / sumlsg)**0.5d0
!        ...THEN THE LOCAL RESIDUAL
!        IF THE RADIUS SPECIFIED FOR THE CALCULATION OF THE LOCAL
!        RESIDUAL IS TOO LOW (TYPICALLY OF THE ORDER OF ONE ELEMENT
!        EDGE), NO NODES HAVE BEEN PROCESSED PREVIOUSLY FOR THE
!        EVALUATION OF SUMLS.
        if (sumlsl .gt. r8prem()) then
            restor = (dlsl / sumlsl)**0.5d0
        else
            restor = 0
!            THE VALUE OF THE RADIUS IS TOO LOW. AN ALARM IS ISSUED.
            call u2mess('F', 'XFEM2_56')
        endif
!
!        PRINT INFORMATION ABOUT THE RESIDUALS
        if (niv .ge. 1) then
            write(ifm,914)i,restor,resglo(i)
            write(ifm,913)
        endif
!
        if (restor .lt. resils) then
            if (niv .ge. 0) then
                write(ifm,*)'   MINIMUM DU RESIDU LOCAL ATTEINT.'
                write(ifm,*)'    ARRET A L''ITERATION ',i
                write(ifm,*)'    RESIDU LOCAL  = ',restor
            endif
            goto 2000
        endif
!        IF (I.GT.5) THEN
!           IF (RESGLO(I).GT.RESGLO(I-1)) THEN
!            WRITE(IFM,*)'GLOBAL RESIDUAL LOWER THAN TOLERANCE ACHIEVED'
!            WRITE(IFM,*)'ITERATIONS DONE = ',I
!            WRITE(IFM,*)'RESIDUAL = ',RESGLO(I)
!              GOTO 2000
!           ENDIF
!        ENDIF
        if (resglo(i) .lt. resigl) then
            if (niv .ge. 0) then
                write(ifm,*)'   MINIMUM LOCAL DU RESIDU GLOBAL ATTEINT.'
                write(ifm,*)'    ARRET A L''ITERATION ',i
                write(ifm,*)'    RESIDU GLOBAL = ',resglo(i)
            endif
            goto 2000
        endif
!
1000  end do
!
2000  continue
!
!     INTEGRATION LOOP ENDED
!
    if (i .ge. itrmax) then
        if (niv .ge. 0) then
            write(ifm,*)'   NOMBRE MAXIMUM D''ITERATION ATTEINT.'
            write(ifm,*)'    NOMBRE D''ITERATIONS = ',i-1
            write(ifm,*)'    RESIDU LOCAL  = ',restor
            write(ifm,*)'    RESIDU GLOBAL = ',resglo(i-1)
        endif
        call u2mess('F', 'XFEM2_65')
    endif
!
!     DESTROY THE TEMPORARY JEVEUX OBJECTS
    call jedetr(tempv)
    call jedetr(cnols)
    call jedetr(celgls)
    call jedetr(chams)
!
    if (reinit .and. (levset.eq.'LT')) then
        call jedetr(poifis)
        call jedetr(trifis)
    endif
!
    call jedetr(forced)
!
    910 format(4x,'+',11('-'),'+',12('-'),'+',12('-'),'+')
    911 format('    | ITERATION |   RESIDU   |   RESIDU   |')
    912 format('    |           |   LOCAL    |   GLOBAL   |')
    913 format(4x,'+',11('-'),'+',12('-'),'+',12('-'),'+')
    914 format(4x,'|',5x,i3,2x,2(' |',e11.4),' | ')
!
!-----------------------------------------------------------------------
!     FIN
!-----------------------------------------------------------------------
    call jedema()
end subroutine
