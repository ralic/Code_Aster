/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF KLOKLO UTILITAI  DATE 29/03/2011   AUTEUR COURTOIS M.COURTOIS */
/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2011  EDF R&D              WWW.CODE-ASTER.ORG */
/*                                                                    */
/* THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR      */
/* MODIFY IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS     */
/* PUBLISHED BY THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE */
/* LICENSE, OR (AT YOUR OPTION) ANY LATER VERSION.                    */
/* THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL,    */
/* BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF     */
/* MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU   */
/* GENERAL PUBLIC LICENSE FOR MORE DETAILS.                           */
/*                                                                    */
/* YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE  */
/* ALONG WITH THIS PROGRAM; IF NOT, WRITE TO : EDF R&D CODE_ASTER,    */
/*    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.     */
/* ================================================================== */
/* ------------------------------------------------------------------ */
#include <sys/types.h>
#include <time.h>
#include "aster.h"

/***************************************************************
*
*  Sous programme       : KLOKLO
*
*	Donne la date ( jour/mois/annee heure:minute:seconde )
*	au moment d'appel de cette fonction.
*
*       Ce sous programme remplace le sous programme KLOAK de la
*       bibliotheque IBM.
*
*	Cette fonction donne les memes informations que la commande date
*       de UNIX.
*
*  Parametres d'entree  :
*  Parametres de sortie :
*
*	date : tableau de 6 entiers dont les significations (FORTRAN)
*	       sont les suivantes
*
*		date(1) : Numero du jour (0-6) de la semaine compte
*                         a partir du Lundi.
*                         Ex si date(1) est 1 alors c'est le Mardi.
*		date(2) : jour (1-28,29,30,31 suivant les mois)
*		date(3) : mois (1-12)
*		date(4) : annee ( qu'il faut additionner avec
*			  1900 pour avoir la vraie valeur )
*		date(5) : heures (0-23)
*		date(6) : minutes (0-59)
*		date(7) : secondes (0-59)
*		date(8) : Numero du jour dans l'annee (1-366)
*		date(9) : Numero de la semaine dans l'annee (1-52)
*
*		
*  Variables globales   :
*
*     - les variables simples
*
*     - les tableaux
*
*  Sous programmes      :
*
*       time
*       localtime
*
*  Exemple d'utilisation
*
*      program tkloklo
*      character*8 jour(7)
*      character*4 mois(12)
*      integer tt(9)
*      data jour/'Lundi', 'Mardi', 'Mercredi', 'Jeudi',
*     &          'Vendredi', 'Samedi', 'Dimanche'/
*      data mois/'Jan','Fev','Mars','Avr','Mai','Juin','Juil','Aout',
*     &          'Sept', 'Oct','Nov','Dec'/
*      call kloklo(tt)
*      print *,jour(tt(1)+1), ', le ', tt(2), ' ', mois(tt(3)),' 19',tt(4),
*     &                     ' a ', tt(5),':',tt(6),':',tt(7)
*      print *,'Nous sommes au ', tt(8),
*     &        'eme jour de l''annee, a la semaine ', tt(9)
*      end
*
*  Auteur      : Albert Y
*
****************************************************************/

void DEFP(KLOKLO, kloklo, INTEGER *date)
{
	time_t timval;
	struct tm *timeptr;

	time(&timval);
	timeptr = localtime(&timval);
	date[6] = (INTEGER)(timeptr->tm_sec);
	date[5] = (INTEGER)(timeptr->tm_min);
	date[4] = (INTEGER)(timeptr->tm_hour);
	date[1] = (INTEGER)(timeptr->tm_mday);
	date[2] = (INTEGER)(timeptr->tm_mon+1);
	date[3] = (INTEGER)(timeptr->tm_year+1900);
   date[0] = (INTEGER)(timeptr->tm_wday == 0 ? 6 : timeptr->tm_wday-1);
   date[7] = (INTEGER)(timeptr->tm_yday+1);
   date[8] = (INTEGER)((date[7]+6)/7);
} 
