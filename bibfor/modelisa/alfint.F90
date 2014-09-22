subroutine alfint(chmatz, imate, nommaz, tdef, noparz,&
                  nummat, prec, ch19)
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
!.======================================================================
    implicit none
!
!      ALFINT   -- INTERPOLATION DES COEFFICIENTS DE DILATATION
!                  ALPHA PERMETTANT LA PRISE EN COMPTE DU FAIT
!                  QUE LA TEMPERATURE DE REFERENCE ( A LAQUELLE
!                  LES DEFORMATIONS SONT NULLES) EST DIFFERENTE
!                  DE LA TEMPERATURE AMBIANTE.
!
!   ARGUMENT        E/S  TYPE         ROLE
!    CHMATZ         IN     K*       NOM DU CHAM_MATER COURANT
!    NOMMAZ         IN     K*       NOM DU MATERIAU COURANT
!    TDEF           IN     R        TEMPERATURE DE DEFINITION DU
!                                   MATERIAU
!    NOPARZ         IN     K*       NOM DU PARAMETRE A INTERPOLER :
!                                   = 'ALPHA'
!                                   = 'FBM_ALPH'
!                                   = 'A_ALPHA'
!    NUMMAT         IN     I        INDICE DU MATERIAU
!    PREC           IN     R        PRECISION AVEC LAQUELLE ON COMPARE
!                                   LA TEMPERATURE COURANTE ET LA
!                                   LA TEMPERATURE DE REFERENCE ET
!                                   SELON LAQUELLE ON DECIDE SI ON
!                                   FAIT UN DEVELOPPEMENT DE T AU
!                                   PREMIER ORDRE AUTOUR DE TREF.
!    CH19Z          VAR    K*       NOM DE LA FONCTION ALPHA DU
!                                   MATERIAU EN ENTREE
!                                   NOM DE LA FONCTION CONTENANT
!                                   LES VALEURS DE ALPHA MODIFIEES EN
!                                   SORTIE
!
!.========================= DEBUT DES DECLARATIONS ====================
! -----  ARGUMENTS
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/gettco.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/gcncon.h"
#include "asterfort/jelira.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jeveut.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/rcvale.h"
#include "asterfort/utmess.h"
!
    character(len=*) :: chmatz, nommaz, noparz
    character(len=19) :: ch19
! -----  VARIABLES LOCALES
    integer :: icodre(1)
    character(len=8) :: k8b, chmat, nommat, ktref, nomgd, valk(2)
    character(len=32) :: phenom
    character(len=16) :: typres, nomcmd, nopara
    character(len=19) :: chwork
    integer :: nummat, ncmp, jnomrc,   i, nbpts, imate
    integer :: nbec, k, ec1, kk, igd, ngdmax,  jvale
    real(kind=8) :: prec, undemi, tref, alfref(1), alphai, ti, tim1, tip1
    real(kind=8) :: alfim1, alfip1, dalref, tdef
    character(len=24), pointer :: prol(:) => null()
    real(kind=8), pointer :: vale(:) => null()
    real(kind=8), pointer :: valw(:) => null()
    integer, pointer :: desc(:) => null()
!
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
! --- INITIALISATIONS :
!     ---------------
    chmat = chmatz
    nommat = nommaz
    nopara = noparz
    undemi = 0.5d0
!
    call getres(k8b, k8b, nomcmd)
!     EN THERMIQUE ON N A PAS BESOIN DE CALCULER ALPHA=F(T)
    if (nomcmd(1:5) .eq. 'THER_') goto 999
!
! --- RECUPERATION DE LA TEMPERATURE DE REFERENCE (TREF):
!     ---------------------------------------------------
!
!     -- NOUVELLE SYNTAXE : AFFE_MATERIAU/AFFE_VARC/TEMP
    call jeveuo(chmat//'.CHAMP_MAT .DESC', 'L', vi=desc)
    call jeveuo(chmat//'.CHAMP_MAT .VALE', 'L', jvale)
    igd = desc(1)
    call jenuno(jexnum('&CATA.GD.NOMGD', igd), nomgd)
    ASSERT(nomgd.eq.'NOMMATER')
    call jelira(jexnom('&CATA.GD.NOMCMP', 'NOMMATER'), 'LONMAX', ncmp)
    call dismoi('NB_EC', nomgd, 'GRANDEUR', repi=nbec)
    ngdmax=desc(2)
!     TREF EST SUR LE 1ER ENTIER CODE :
    ec1=desc(3+2*ngdmax+nbec*(imate-1)+1)
    k=0
    do kk = 1, 30
        if (exisdg([ec1],kk)) k=k+1
    end do
    if (zk8(jvale+ncmp*(imate-1)+k-2) .ne. 'TREF=>') then
        call utmess('F', 'CALCULEL6_56', sk=chmat)
    endif
    ktref = zk8(jvale+ncmp*(imate-1)+k-1)
    if (ktref .eq. 'NAN') goto 9998
!
    read (ktref,'(F8.2)') tref
!
! --- RECUPERATION DU NOM DU PHENOMENE ASSOCIE AU MATERIAU :
!     ----------------------------------------------------
    call jeveut(nommat//'.MATERIAU.NOMRC', 'L', jnomrc)
    phenom = zk32(jnomrc+nummat-1)(1:10)
!
! --- CREATION DE LA NOUVELLE FONCTION DEVANT CONTENIR LES VALEURS
! --- INTERPOLEES DE ALPHA :
! --- LE NOM DE CETTE NOUVELLE FONCTION EST DEFINI DE LA MANIERE
! --- SUIVANTE : NOM_NOUVEAU = &&NOM_ANCIEN(1:2)//00NUMMAT :
!     ----------------------------------------------------
    call gcncon('.', chwork)
    chwork = '&&'//chwork(3:8)
!
! --- CREATION DE LA NOUVELLE FONCTION CHWORK PAR RECOPIE DE CH19
! --- SUR LA VOLATILE :
    call gettco(ch19, typres)
    if (typres .eq. 'FORMULE') then
        call utmess('F', 'MODELISA2_1')
    endif
    call copisd('FONCTION', 'V', ch19, chwork)
!
! --- RECUPERATION DU NOMBRE DE POINTS DEFINISSANT LA FONCTION :
    call jelira(chwork(1:19)//'.VALE', 'LONMAX', nbpts)
!
! --- NOMBRE DE POINTS DE LA FONCTION :
    nbpts = nbpts/2
!
!     -- SI LA FONCTION N'A QU'UN POINT :
    if (nbpts .eq. 1) then
        call jeveuo(chwork(1:19)//'.PROL', 'L', vk24=prol)
!        -- SI LA FONCTION EST UNE CONSTANTE, ON NE FAIT RIEN :
        if (prol(1) .eq. 'CONSTANT') then
            goto 999
!        -- SI TREF ET TDEF SONT PROCHES (1 DEGRE), ON NE FAIT RIEN :
        else if (abs(tref-tdef).lt.1.d0) then
            goto 999
!        -- SINON ON ARRETE TOUT :
        else
            call utmess('F', 'MODELISA2_42', sk=ch19(1:8))
        endif
    endif
!
! --- CALCUL DE ALPHA A LA TEMPERATURE DE REFERENCE :
    call rcvale(nommat, phenom, 1, 'TEMP    ', [tref],&
                1, nopara, alfref(1), icodre(1), 2)
!
! --- RECUPERATION DU .VALE DE LA FONCTION DESTINEE A CONTENIR LES
! --- VALEURS DE ALPHA INTERPOLEES :
    call jeveuo(chwork(1:19)//'.VALE', 'E', vr=valw)
!
! --- RECUPERATION DU .VALE DE LA FONCTION CONTENANT LES
! --- VALEURS INITIALES DE ALPHA  :
    call jeveuo(ch19(1:19)//'.VALE', 'L', vr=vale)
!
! --- CALCUL DES ALPHA INTERPOLES :
    do i = 1, nbpts
!
        alphai = vale(1+i+nbpts-1)
        ti = vale(i)
!
! --- DANS LE CAS OU ABS(TI-TREF) > PREC :
! --- ALPHA_NEW(TI) = (ALPHA(TI)*(TI-TDEF) - ALPHA(TREF)*(TREF-TDEF))
! ---                 /(TI-TREF)   :
        if (abs(ti-tref) .ge. prec) then
!
            valw(1+i+nbpts-1) = ( alphai*(ti-tdef)- alfref(1)*(tref- tdef)) /(ti-tref )
! --- DANS LE CAS OU ABS(TI-TREF) < PREC :
! --- IL FAUT D'ABORD CALCULER LA DERIVEE DE ALPHA PAR RAPPORT
! --- A LA TEMPERATURE EN TREF : D(ALPHA)/DT( TREF) :
        else
! ---   DANS LE CAS OU I > 1 ET I < NBPTS :
! ---   D(ALPHA)/DT( TREF) = 0.5*((ALPHA(TI+1)-ALPHA(TREF))/(TI+1-TREF)
! ---                            +(ALPHA(TREF)-ALPHA(TI-1))/(TREF-TI-1))
            if (i .gt. 1 .and. i .lt. nbpts) then
!
                tim1 = vale(1+i-1-1)
                tip1 = vale(1+i+1-1)
                alfim1 = vale(1+i+nbpts-1-1)
                alfip1 = vale(1+i+nbpts+1-1)
                if (tip1 .eq. tref) then
                    call utmess('F', 'MODELISA2_2')
                endif
                if (tim1 .eq. tref) then
                    call utmess('F', 'MODELISA2_2')
                endif
!
                dalref = undemi*((alfip1-alfref(1))/(tip1-tref) +(alfref(1)- alfim1)/(tref-tim1))
!
! ---   DANS LE CAS OU I = NBPTS :
! ---   D(ALPHA)/DT( TREF) = (ALPHA(TREF)-ALPHA(TI-1))/(TREF-TI-1) :
            else if (i.eq.nbpts) then
!
                tim1 = vale(1+i-1-1)
                alfim1 = vale(1+i+nbpts-1-1)
                if (tim1 .eq. tref) then
                    call utmess('F', 'MODELISA2_2')
                endif
!
                dalref = (alfref(1)-alfim1)/(tref-tim1)
!
! ---   DANS LE CAS OU I = 1 :
! ---   D(ALPHA)/DT( TREF) = (ALPHA(TI+1)-ALPHA(TREF))/(TI+1-TREF) :
            else if (i.eq.1) then
!
                tip1 = vale(1+i+1-1)
                alfip1 = vale(1+i+nbpts+1-1)
                if (tip1 .eq. tref) then
                    call utmess('F', 'MODELISA2_2')
                endif
!
                dalref = (alfip1-alfref(1))/(tip1-tref)
!
            endif
! ---   DANS CE CAS OU ABS(TI-TREF) < PREC , ON A :
! ---   ALPHA_NEW(TI) = ALPHA_NEW(TREF)
! ---   ET ALPHA_NEW(TREF) = D(ALPHA)/DT (TREF)*(TREF-TDEF)+ALPHA(TREF):
            valw(1+i+nbpts-1) = dalref*(tref-tdef) + alfref(1)
!
        endif
!
    end do
!
! --- ON REMPLACE LA FONCTION EN ENTREE CH19 PAR LA FONCTION
! --- DE TRAVAIL CONTENANT LES VALEURS DE ALPHA INTERPOLEES CHWORK :
    ch19 = chwork
!
    goto 999
!     -- SECTION "ERREUR":
9998 continue
    valk(1)=chmat
    valk(2)=nommat
    call utmess('F', 'CALCULEL6_1', nk=2, valk=valk)
!
999 continue
end subroutine
