%{
#include <stdio.h>

void yyerror (const char *s) 
{ /* Called by yyparse on error */
	printf ("%s\n", s); }
%}



%token tMAIL
%token tENDMAIL 
%token tSCHEDULE
%token tENDSCH
%token tSEND
%token tSET
%token tTO
%token tFROM
%token tAT
%token tCOMMA
%token tCOLON
%token tLPR
%token tRPR
%token tLBR
%token tRBR
%token tIDENT
%token tSTRING
%token tDATE
%token tTIME
%token tADDRESS


%%

program  :
		| mailBlock program 
		| setStatement program 
		;
		
mailBlock :     tMAIL tFROM tADDRESS tCOLON statementList tENDMAIL
                |tMAIL tFROM tADDRESS tCOLON tENDMAIL
                ;
statementList :   setStatement statementList
		  |sendStatement statementList
		  |scheduleStatement statementList 
		  |sendStatement 
		  |scheduleStatement
		  |setStatement 
		  ;
setStatement : 	tSET tIDENT tLPR tSTRING tRPR
		;		
sendStatement :	tSEND tLBR tIDENT tRBR tTO recipientList	
	        |tSEND tLBR tSTRING tRBR tTO recipientList
               ;

sendStatementList: sendStatement 
		   |sendStatement  sendStatementList
                   ;
scheduleStatement: tSCHEDULE tAT tLBR tDATE tCOMMA tTIME tRBR tCOLON sendStatementList tENDSCH
		  ;
recipientObject : tLPR tADDRESS tRPR
	          | tLPR tSTRING tCOMMA tADDRESS tRPR
	          | tLPR tIDENT tCOMMA tADDRESS tRPR
		 ;
recipientObjectList : recipientObject tCOMMA recipientObject
		     | recipientObject tCOMMA recipientObjectList
		    ;
recipientList : tLBR recipientObject tRBR
		|tLBR recipientObjectList tRBR 
	      ;


%%

int main ()
{
   if (yyparse()) {
   // parse error
       printf("ERROR\n");
       return 1;
   }
   else {
   // successful parsing
      printf("OK\n");
      return 0;
   }
}
